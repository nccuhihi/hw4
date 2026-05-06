# 參考文獻 (Reference):
# 1. AI Assistant (Gemini):https://gemini.google.com/share/ea1d5af0aaa9
# 2. AI Assistant (Perplexity):https://www.perplexity.ai/search/c2f87ea3-e4e4-4f2b-b853-e65449423be5

#install.packages(c("shiny", "shinythemes", "ggplot2", "DT", "FactoMineR", "factoextra"))
#install.packages("remotes")         # 安裝 remotes（比 devtools 快很多）
#remotes::install_github("vqv/ggbiplot")  # 安裝 ggbiplot

library(shiny)
library(shinythemes)
library(ggplot2)
library(ggbiplot)
library(DT)
library(FactoMineR)
library(factoextra)

# ----------------- 資料前處理 -----------------
data(iris)
# 取前四個數值變數並取 log
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

# 建立 PCA 模型 (預設 scale. = TRUE)
ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)

# 建立 CA 模型 (將連續變數離散化以建立列聯表，此作法在 CA 中更具統計意義)
iris_disc <- iris
iris_disc$Petal.Length.cat <- cut(iris$Petal.Length, breaks = 3, labels = c("短", "中", "長"))
ca_table <- table(iris$Species, iris_disc$Petal.Length.cat)
res.ca <- CA(ca_table, graph = FALSE)

# 自訂的 UI 元件：類似資訊方塊的美化排版
valueBoxLike <- function(value, label) {
  div(
    style = "background:#ECF0F1; border-radius:8px; padding:20px; text-align:center; margin:5px;",
    h2(value, style = "color:#2980B9; margin:0;"),
    p(label, style = "color:#7F8C8D; margin:0;")
  )
}

# ----------------- UI 介面設計 -----------------
ui <- navbarPage(
  theme = shinytheme("flatly"), # 套用美觀的主題 (Aesthetics)
  title = "🌸 PCA & CA Interactive Analysis",
  
  # 分頁 1：基本資訊與資料 (Base Task 1)
  tabPanel("🏠 Home (基本資訊)",
           fluidPage(
             wellPanel(
               h3("👤 Student Information"),
               # !!! 請在此處修改您的姓名與學號 !!!
               p(strong("Name: "), "賴昱瑋"), 
               p(strong("Student ID: "), "114971009"),
               p(strong("ShinyApps Link: "), a("https://allenlai.shinyapps.io/NCCU_DS2024_hw4_114971009/", href = "#", target = "_blank"))
             ),
             h3("📊 Iris Dataset Overview"),
             p("本作業使用 R 內建的 Iris 資料集，共 150 筆觀測值，4 個數值特徵（Sepal/Petal Length & Width），3 種鳶尾花品種。",
               "PCA 使用四個特徵取 log 後做標準化主成分分析；",
               "CA 使用品種 × 花瓣長度分類的列聯表進行對應分析。"),
             fluidRow(
               column(4, valueBoxLike("150", "Total Observations")),
               column(4, valueBoxLike("4", "Numeric Features")),
               column(4, valueBoxLike("3", "Species"))
             ),
             hr(),
             h3("📋 Input Data（原始資料集）"),
             p("以下為本次分析所使用的原始 Iris 資料集。可利用欄位標頭排序或搜尋框篩選觀測值。"),
             DTOutput("iris_table") # 互動式資料表
           )
  ),
  
  # 分頁 2：PCA 分析 (Base Task 2)
  tabPanel("🔵 PCA（主成分分析）",
           sidebarLayout(
             sidebarPanel(width = 3,
                          h4("⚙️ PCA 設定"),
                          selectInput("pc_x", "X 軸主成分：",
                                      choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4),
                                      selected = 1),
                          selectInput("pc_y", "Y 軸主成分：",
                                      choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3, "PC4" = 4),
                                      selected = 2),
                          hr(),
                          checkboxInput("show_ellipse", "顯示群組橢圓 (Ellipse)", value = TRUE),
                          checkboxInput("show_arrows", "顯示變數箭頭 (Biplot Arrows)", value = TRUE),
                          hr(),
                          helpText("提示：請選擇不同的主成分來觀察資料在降維後的分布狀況。"),
                          hr(),
                          h5("📌 Variance Explained"),
                          tableOutput("pca_variance_sidebar")
             ),
             mainPanel(width = 9,
                       tabsetPanel(
                         tabPanel("PCA Biplot", 
                                  br(), plotOutput("pca_plot", height = "500px")),
                         tabPanel("Scree Plot（陡坡圖）", 
                                  br(), plotOutput("pca_scree", height = "400px")),
                         tabPanel("Loadings（變數負荷量）", 
                                  br(), plotOutput("pca_loadings", height = "400px")),
                         tabPanel("Variance Explained（變異解釋率）", 
                                  br(), 
                                  h5("各主成分解釋變異量（完整版）"),
                                  tableOutput("pca_variance_table"),
                                  br(),
                                  h5("PCA Scores（主成分分數）"),
                                  DTOutput("pca_scores_table"))
                       )
             )
           )
  ),
  
  # 分頁 3：CA 分析 (Base Task 3)
  tabPanel("🟢 CA（對應分析）",
           sidebarLayout(
             sidebarPanel(width = 3,
                          h4("⚙️ CA 設定"),
                          selectInput("ca_dim_x", "X 軸維度：", choices = c("Dim 1" = 1, "Dim 2" = 2), selected = 1),
                          selectInput("ca_dim_y", "Y 軸維度：", choices = c("Dim 1" = 1, "Dim 2" = 2), selected = 2),
                          hr(),
                          h5("📌 Contingency Table (列聯表)"),
                          p("鳶尾花品種 × 花瓣長度分組"),
                          tableOutput("ca_contingency"),
                          hr(),
                          helpText("CA 將列聯表中的行（品種）與列（花瓣長度類別）映射到低維空間，以探索兩者之間的關聯結構。")
             ),
             mainPanel(width = 9,
                       tabsetPanel(
                         tabPanel("CA Biplot", 
                                  br(), plotOutput("ca_plot", height = "500px")),
                         tabPanel("Row/Col Plots", 
                                  br(), 
                                  fluidRow(
                                    column(6, plotOutput("ca_row_plot", height = "380px")),
                                    column(6, plotOutput("ca_col_plot", height = "380px"))
                                  )),
                         tabPanel("Scree Plot（陡坡圖）", 
                                  br(), plotOutput("ca_scree", height = "400px"))
                       )
             )
           )
  )
)

# ----------------- Server 邏輯設計 -----------------
server <- function(input, output, session) {
  
  # 渲染資料表 (使用 DT 提供搜尋、排序等互動功能)
  output$iris_table <- renderDT({
    datatable(iris, filter = "top", 
              options = list(pageLength = 10, scrollX = TRUE),
              class = "stripe hover compact")
  })
  
  # ----------------- PCA 相關輸出 -----------------
  output$pca_plot <- renderPlot({
    x_choice <- as.numeric(input$pc_x)
    y_choice <- as.numeric(input$pc_y)
    
    # 基礎防呆：若 X 與 Y 軸選擇相同
    if (x_choice == y_choice) {
      plot.new()
      text(0.5, 0.5, "⚠️ 請選擇不同的主成分作為 X 軸與 Y 軸", cex = 1.3, col = "red")
      return()
    }
    
    # 使用 ggbiplot 繪製，加入 checkbox 互動控制
    g <- ggbiplot(ir.pca, choices = c(x_choice, y_choice), 
                  obs.scale = 1, var.scale = 1, 
                  groups = ir.species, 
                  ellipse = input$show_ellipse, 
                  circle = TRUE,
                  var.axes = input$show_arrows)
    g <- g + scale_color_discrete(name = 'Species')
    g <- g + theme_minimal(base_size = 13) + theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
  
  # 輸出 PCA Scree Plot
  output$pca_scree <- renderPlot({
    fviz_eig(ir.pca, addlabels = TRUE, ylim = c(0, 80)) + 
      theme_minimal(base_size = 13) +
      ggtitle("Scree Plot：各主成分解釋變異比例")
  })
  
  # 輸出 PCA Loadings
  output$pca_loadings <- renderPlot({
    fviz_pca_var(ir.pca, col.var = "contrib",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE) + 
      theme_minimal(base_size = 13) +
      ggtitle("Variable Contributions (Loadings)")
  })
  
  # 輸出 PCA 變異解釋率表格
  output$pca_variance_table <- renderTable({
    variance <- summary(ir.pca)$importance
    data.frame(
      Principal_Component = colnames(variance),
      Standard_Deviation = round(variance[1, ], 4),
      Proportion_of_Variance = paste0(round(variance[2, ] * 100, 2), "%"),
      Cumulative_Proportion = paste0(round(variance[3, ] * 100, 2), "%")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # 輸出 PCA 側邊欄精簡版變異解釋率
  output$pca_variance_sidebar <- renderTable({
    v <- summary(ir.pca)$importance
    data.frame(
      PC = colnames(v),
      Prop = paste0(round(v[2, ] * 100, 1), "%"),
      Cum = paste0(round(v[3, ] * 100, 1), "%")
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # 輸出 PCA Scores 互動資料表
  output$pca_scores_table <- renderDT({
    scores <- as.data.frame(round(ir.pca$x, 4))
    scores$Species <- ir.species
    datatable(scores, filter = "top", 
              options = list(pageLength = 10, scrollX = TRUE),
              class = "stripe hover compact")
  })
  
  # ----------------- CA 相關輸出 -----------------
  # 繪製 CA 圖
  output$ca_plot <- renderPlot({
    axes_choice <- c(as.numeric(input$ca_dim_x), as.numeric(input$ca_dim_y))
    # 使用 factoextra 繪製美觀的 CA 雙序圖
    fviz_ca_biplot(res.ca, axes = axes_choice, repel = TRUE, 
                   title = "CA Biplot：Species vs Petal Length Category") + 
      theme_minimal(base_size = 13)
  })
  
  output$ca_row_plot <- renderPlot({
    axes_choice <- c(as.numeric(input$ca_dim_x), as.numeric(input$ca_dim_y))
    fviz_ca_row(res.ca, axes = axes_choice, repel = TRUE, col.row = "#E74C3C") + 
      theme_minimal(base_size = 12) +
      ggtitle("Row Points (Species)")
  })
  
  output$ca_col_plot <- renderPlot({
    axes_choice <- c(as.numeric(input$ca_dim_x), as.numeric(input$ca_dim_y))
    fviz_ca_col(res.ca, axes = axes_choice, repel = TRUE, col.col = "#3498DB") + 
      theme_minimal(base_size = 12) +
      ggtitle("Column Points (Petal Length Cat)")
  })
  
  output$ca_scree <- renderPlot({
    fviz_eig(res.ca, addlabels = TRUE) + 
      theme_minimal(base_size = 13) +
      ggtitle("CA Scree Plot：各維度解釋變異比例")
  })
  
  output$ca_contingency <- renderTable({
    as.data.frame.matrix(ca_table)
  }, rownames = TRUE, striped = TRUE, bordered = TRUE)
  
}

# 執行 Shiny 應用程式
shinyApp(ui = ui, server = server)