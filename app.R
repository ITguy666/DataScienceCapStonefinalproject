# ==============================================================================
# Coursera Data Science Capstone: Shiny Application
# File: app.R
# ==============================================================================

library(shiny)
library(bslib)
library(data.table)
library(stringr)

# Load Prepared N-Gram Lookup Tables
ngram_model <- readRDS("ngram_model.rds")
bi_dt   <- ngram_model$bi
tri_dt  <- ngram_model$tri
quad_dt <- ngram_model$quad

# Stupid Back-off Prediction Logic
predict_next_word <- function(input_text, top_n = 3) {
        # Clean input text
        clean_input <- tolower(input_text)
        clean_input <- gsub("[^a-z' ]", "", clean_input)
        words <- unlist(strsplit(trimws(clean_input), "\\s+"))
        
        if (length(words) == 0 || input_text == "") {
                return(data.table(target = c("the", "on", "a"), score = c(0.1, 0.08, 0.05)))
        }
        
        n <- length(words)
        
        # 1. Try Quadgram (matching last 3 words)
        if (n >= 3) {
                ctx <- paste(tail(words, 3), collapse = " ")
                match <- quad_dt[context == ctx][order(-count)]
                if (nrow(match) > 0) {
                        res <- match[1:min(nrow(match), top_n), .(target, score = count * 1.0)]
                        return(res)
                }
        }
        
        # 2. Back-off to Trigram (matching last 2 words)
        if (n >= 2) {
                ctx <- paste(tail(words, 2), collapse = " ")
                match <- tri_dt[context == ctx][order(-count)]
                if (nrow(match) > 0) {
                        res <- match[1:min(nrow(match), top_n), .(target, score = count * 0.4)]
                        return(res)
                }
        }
        
        # 3. Back-off to Bigram (matching last 1 word)
        if (n >= 1) {
                ctx <- tail(words, 1)
                match <- bi_dt[context == ctx][order(-count)]
                if (nrow(match) > 0) {
                        res <- match[1:min(nrow(match), top_n), .(target, score = count * 0.16)]
                        return(res)
                }
        }
        
        # Default Fallback (Unigram / Most Common Words)
        return(data.table(target = c("the", "to", "and"), score = c(0.01, 0.01, 0.01)))
}

# UI Definition
ui <- page_sidebar(
        theme = bs_theme(
                version = 5,
                bootswatch = "zephyr",
                primary = "#4f46e5"
        ),
        title = "Next Word Predictor | DS Capstone",
        
        sidebar = sidebar(
                title = "Application Controls",
                textInput("user_text", "Enter Text Phrase:", value = "How are you", placeholder = "Type a phrase..."),
                sliderInput("num_predictions", "Max Suggestions:", min = 1, max = 5, value = 3),
                hr(),
                helpText("The prediction engine uses an N-Gram model with Stupid Back-off strategy trained on millions of news, blog, and tweet sentences.")
        ),
        
        layout_columns(
                card(
                        card_header("Top Next Word Prediction"),
                        div(style = "text-align: center; padding: 20px;",
                            h2(uiOutput("top_prediction"), style = "color: #4f46e5; font-size: 42px; font-weight: bold;"),
                            p(uiOutput("confidence_score"), style = "color: #64748b;")
                        )
                ),
                card(
                        card_header("Alternative Candidate Words"),
                        tableOutput("candidates_table")
                )
        )
)

# Server Logic
server <- function(input, output, session) {
        
        predictions <- reactive({
                req(input$user_text)
                predict_next_word(input$user_text, top_n = input$num_predictions)
        })
        
        output$top_prediction <- renderText({
                res <- predictions()
                if (nrow(res) > 0) toupper(res$target[1]) else "..."
        })
        
        output$confidence_score <- renderText({
                res <- predictions()
                if (nrow(res) > 0) paste("Back-off Relative Score:", round(res$score[1], 3)) else ""
        })
        
        output$candidates_table <- renderTable({
                res <- predictions()
                colnames(res) <- c("Predicted Word", "Score")
                res
        }, striped = TRUE, hover = TRUE, width = "100%")
}

# Run Application
shinyApp(ui = ui, server = server)


