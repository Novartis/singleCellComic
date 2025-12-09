# ==============================================================================
#  Load all necessary libraries for the app
# ==============================================================================
library(shiny)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(patchwork)
library(Seurat)
library(harmony)
library(purrr)
library(tibble)
library(scales)
library(stringr)

# ==============================================================================
#  Source the functions from the separate file
# ==============================================================================
source("functions.R")

# ==============================================================================
#  SHINY APP UI
# ==============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    /* CSS Rule to center the navigation tabs */
    .nav-tabs {
      display: flex;
      justify-content: center;
    }

    /* Add padding to the bottom of the page */
    .container-fluid {
        padding-bottom: 50px;
    }
    
    #start_screen {
        max-width: 600px;
        margin: 50px auto;
        padding: 20px;
        border: 1px solid #ccc;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        text-align: center; 
    }

    .shiny-output-error-validation { color: #ff0000; font-weight: bold; }
    .well { background-color: #f0f8ff; border: 1px solid #d4e8ff; }
    .btn-lg { font-size: 1.2em; padding: 10px 16px; }
    h2, h3, h4 { color: #2c3e50; }
    .conclusion-box { padding: 15px; background-color: #f8f9fa; border-left: 5px solid #5bc0de; margin-top: 20px; }
    #act1_console_output, #unintegrated_console_output, #integrated_console_output, #label_transfer_console_output, #clustering_console_output { 
      background-color: #272822; 
      color: #F8F8F2;
      border-radius: 5px;
      padding: 10px;
      font-family: Menlo, Monaco, Consolas, 'Courier New', monospace;
      font-size: 0.9em;
      white-space: pre-wrap;
    }
  "))),
  
  tags$img(src = "header_logo.png", 
           style = "display: block; margin-left: auto; margin-right: auto; max-width: 800px; height: auto; margin-bottom: 20px;"),
  
  titlePanel(h1("The singleCellComic Game", align = "center")),
  
  uiOutput("app_ui")
)

# ==============================================================================
#  SHINY APP SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    game_ready = FALSE, 
    
    # Placeholders for data
    b1_data = NULL, b2_data = NULL, b3_data = NULL,
    act1_seurat = NULL,
    unintegrated_seurat = NULL,
    integrated_seurat_soft = NULL,
    integrated_seurat_medium = NULL,
    integrated_seurat_strong = NULL,
    clustered_seurat_soft = NULL,
    clustered_seurat_strong = NULL,
    label_transfer_results = NULL,
    
    # Display logs
    act1_log_display = "",
    clustering_log_display = "",
    
    # Cached full log messages
    act1_log_complete = "",
    clustering_log_complete = "",
    
    # State variables
    act4_feedback = NULL,
    act2_q1_feedback = NULL,
    act2_q2_feedback = NULL,
    act2_q3_feedback = NULL,
    act2_q1_correct = FALSE,
    act2_q2_correct = FALSE,
    act2_q3_correct = FALSE,
    act4_q1_feedback = NULL,
    act4_q1_correct = FALSE,
    act4_q2_feedback = NULL,
    game_cells = NULL,
    game_guesses = character(0),
    current_game_cell_idx = 1,
    game_finished = FALSE,
    label_map = NULL,
    guess_feedback = NULL,
    
    # Flags to prevent re-running computations within a session
    act1_already_run = FALSE,
    unintegrated_already_run = FALSE,
    integrated_already_run = FALSE,
    clustered_already_run = FALSE,
    act2_integrations_complete = FALSE
  )
  
  # ============================================================================
  # DYNAMIC UI LOGIC: START SCREEN -> GAME
  # ============================================================================
  output$app_ui <- renderUI({
    if (!rv$game_ready) {
      div(
        id = "start_screen",
        h2("Game Setup"),
        p("Choose a mode and set a seed to begin."),
        hr(),
        div(style="display: inline-block; text-align: left; margin-bottom: 15px;", 
            radioButtons("game_mode", "Select Mode:", 
                         choices = c("Caching (faster startup)", "Live (on-the-fly calculations)"),
                         selected = "Caching (faster startup)")
        ),
        div(style="display: inline-block; text-align: left;",
            numericInput("seed", "Set Seed for Reproducibility:", value = 123, min = 1)
        ),
        br(),
        actionButton("start_game", "Start Game!", class = "btn-success btn-lg", style="margin-top: 10px;")
      )
    } else {
      # Main game UI (dynamically loaded)
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel("Characters",
                 fluidRow(column(12, h2("Meet the Cells of the Immune System", align = "center"), p("In the world of single-cell RNA sequencing, every cell has a story told by its gene expression. Here, we imagine each cell type as a unique character, defined by a distinct combination of features. Get to know our main characters before the story begins.", align="center"))),
                 hr(),
                 lapply(c("T-Cell", "B-Cell", "Macrophage", "Dendritic-Cell"), function(ct) {
                   div(
                     h4(ct, align = "center"),
                     p(
                       switch(ct,
                              "T-Cell" = "A highly variable cell type, recognized by their reddish color and spiky hair. In RNA-seq terms, this means they share a core set of 'T-cell genes' but have many other genes that vary, hinting at different subtypes. Notice their expressions—some look calm, others aggressive.",
                              "B-Cell" = "Primarily defined by its large headphones and moustache, representing a very strong and unique gene expression signature. This cell is generally bald and has a smiling or neutral expression.",
                              "Macrophage" = "The key feature is its large, prominent moustache. It has a distinct reddish-orange color, a new afro hairstyle, and a consistently neutral expression. This combination makes it easy to spot in a crowd.",
                              "Dendritic-Cell" = "Defined by its unique combination of long, flowing hair and a tendency to wear sunglasses. This represents a specialized set of genes that are active, giving it a unique function and appearance."
                       ), align = "center"
                     ),
                     withSpinner(plotOutput(paste0(ct, "_intro_plot"), height = "150px")),
                     hr()
                   )
                 })
        ),
        
        tabPanel("Act 1: A Peaceful World",
                 fluidRow(
                   column(12, 
                          h2("The Cells Coexist Peacefully", align = "center"),
                          p("Our story begins in a perfectly controlled environment—a single blood sample we'll call 'Batch 1'. In the world of RNA-seq, this is like analyzing data from one experiment, where all technical factors are consistent. The cells' unique features (their 'gene expression') cause them to cluster into distinct populations. We can visualize these relationships using a UMAP plot, a powerful technique that places cells with similar features closer together, much like a social map.", align = "center")
                   )
                 ),
                 fluidRow(
                   column(12, align="center",
                          withSpinner(plotOutput("act1_example_cells", height="150px"))
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(12, align="center",
                          actionButton("run_act1_umap", "Visualize Cell Populations!", class = "btn-primary btn-lg")
                   )
                 ),
                 hr(),
                 conditionalPanel(
                   condition = "input.run_act1_umap > 0",
                   fluidRow(
                     column(8, withSpinner(plotOutput("act1_umap_celltype"))),
                     column(4, 
                            h4("Analysis Log"),
                            verbatimTextOutput("act1_console_output")
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "output.act1_umap_celltype",
                   hr(),
                   div(style="background-color: #fdfefe; padding: 20px; border: 1px solid #e5e8e8; border-radius: 5px;",
                       h3("Exploring the 'Genes' Behind the Features", align="center"),
                       p("The UMAP shows us the big picture, but what about the individual 'genes' that define these cell types? Let's investigate. Select a feature below to see its underlying gene expression pattern.", align="center"),
                       fluidRow(
                         column(12, align="center",
                                selectInput("act1_feature_select", "Select a Feature (Gene) to Visualize:",
                                            choices = c("Headphones" = "headphones.size",
                                                        "Moustache" = "moustache.size",
                                                        "Hairstyle" = "hairstyle.value",
                                                        "Hair Length" = "hair.length",
                                                        "Sunglasses" = "sunglasses.size",
                                                        "Mouth Expression" = "mouth.expression.value"))
                         )
                       ),
                       fluidRow(
                         column(6, plotOutput("act1_feature_plot")),
                         column(6, plotOutput("act1_vln_plot"))
                       ),
                       hr(),
                       h4("How 'Gene Expression' Creates a Feature", align="center"),
                       p("A gene's numeric expression value can translate to a visual feature in different ways:", align="center"),
                       fluidRow(
                         column(6,
                                h5("Proportional Mapping", align="center"),
                                p(strong("Biological Example:"), "A T-cell's surface protein CD8. The amount of CD8 mRNA directly relates to the density of the protein on the cell surface. More RNA = more protein. This is a common, direct relationship in biology.", br(), "In our comic, this is like 'moustache.size':", align="center"),
                                withSpinner(plotOutput("prop_mapping_plot", height="150px"))
                         ),
                         column(6,
                                h5("Categorical Mapping", align="center"),
                                p(strong("Biological Example:"), "The Sonic hedgehog (Shh) gene in neural development. Cells exposed to a high concentration of the Shh protein become one cell type (floor plate cells), while cells seeing a medium or low concentration become entirely different types of neurons (V3 interneurons or motor neurons, respectively). One gene's expression level, three distinct fates: 'apple', 'orange', 'banana'.", br(), "In our comic, this is like 'hairstyle.value':", align="center"),
                                withSpinner(plotOutput("cat_mapping_plot", height="150px"))
                         )
                       )
                   ),
                   hr(),
                   fluidRow(
                     column(12, align="center",
                            div(class="conclusion-box",
                                h3("Act 1 Conclusion"),
                                p("Success! The UMAP plot clearly separates the cell types, and by exploring individual genes with Feature and Violin plots, we can see the specific expression patterns that define them. This is the ideal outcome, where biological differences are the only thing driving the clustering. But what happens when we need to compare cells from different environments?"),
                                p(strong("In the next act, a viral invasion will force us to combine data from a new source, revealing a hidden challenge.")),
                                actionButton("goToAct2", "Begin Act 2: The Viral Invasion", class="btn-info")
                            )
                     )
                   )
                 )
        ),
        
        tabPanel("Act 2: The Viral Invasion",
                 fluidRow(column(12, h2("A Viral Invasion!", align="center"), p("A virus has invaded a nearby tissue! Our brave immune cells from the blood (Batch 1) rush to help. There, they encounter immune cells from a different location (Batch 2). In RNA-seq, this is like combining datasets from different conditions—for example, from two different labs or patients. Crucially, these different conditions often involve small, unavoidable technical variations (like different sample preparation kits or sequencer calibrations) that can systematically alter the data. This is the source of a 'batch effect'.", align="center"))),
                 hr(),
                 fluidRow(
                   column(6, h4("Batch 1 (Blood)", align = "center"), withSpinner(plotOutput("batch1_example", height="300px"))),
                   column(6, h4("Batch 2 (Tissue)", align = "center"), withSpinner(plotOutput("batch2_example", height="300px")))
                 ),
                 hr(),
                 
                 fluidRow(
                   column(12, align="center",
                          h4("What's the main, consistent difference between Batch 1 and Batch 2 cells?"),
                          actionButton("act2_guess_moustache", "Moustache Size"),
                          actionButton("act2_guess_headphones", "Headphones"),
                          actionButton("act2_guess_rollerblades", "Rollerblades"),
                          actionButton("act2_guess_hair", "Hair Style"),
                          uiOutput("act2_q1_feedback")
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.act2_guess_rollerblades > 0 && output.act2_q1_correct == true",
                   hr(),
                   fluidRow(
                     column(12, align="center",
                            h4("Is this strong 'rollerblade' feature a true biological difference or a technical artifact?"),
                            actionButton("act2_guess_biological", "Biological Effect"),
                            actionButton("act2_guess_batch", "Batch Effect"),
                            uiOutput("act2_q2_feedback")
                     )
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.act2_guess_batch > 0 && output.act2_q2_correct == true",
                   hr(),
                   fluidRow(
                     column(12, align="center",
                            p("Exactly. Let's combine the data from both batches and see what happens when we don't account for this batch effect."),
                            actionButton("run_unintegrated", "Combine and Visualize Data", class = "btn-primary btn-lg")
                     )
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.run_unintegrated > 0",
                   hr(),
                   fluidRow(
                     column(12, h3("The Matching Challenge", align="center"), p("The UMAP is a disaster! The cells are clearly separated by batch. The 'rollerblade' feature is so strong that it's impossible to tell which populations from Batch 1 correspond to those in Batch 2. The labels have been anonymized to reflect this confusion. Can you guess which populations are the same cell type?", align="center"))
                   ),
                   fluidRow(
                     column(8, withSpinner(plotOutput("umap_unintegrated_anonymous"))),
                     column(4, 
                            h4("Match the Populations"),
                            uiOutput("matching_game_ui"),
                            br(),
                            actionButton("lock_guesses", "Submit my mapping!", class="btn-warning btn-lg")
                     )
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.lock_guesses > 0",
                   hr(),
                   fluidRow(
                     column(12, align="center",
                            h3("Solution: Data Integration", align="center"),
                            p("That was tricky! Batch effects make visual matching unreliable. To find the true relationships, we must perform data integration. Let's use Harmony - a very popular tool for scRNA data integration - to correct for the technical variation. Harmony has a key parameter, 'theta', which controls how aggressively it merges different batches. Let's see what happens with different theta values."),
                            actionButton("run_integrated", "Run Harmony Integration!", class = "btn-success btn-lg")
                     )
                   ),
                   conditionalPanel(
                     condition = "input.run_integrated > 0",
                     hr(),
                     fluidRow(
                       column(12, h3("The Reveal!", align="center"), p("Success! After integration, the batch effect is reduced. Use the tabs below to explore the results of different integration strengths. 'Soft' integration (low theta) is gentle, while 'Strong' integration (high theta) is very aggressive. Notice how the populations you were trying to match are now better overlapped.", align="center"))
                     ),
                     tabsetPanel(
                       id = "integration_tabs",
                       tabPanel("Soft Integration (theta=0.05)", 
                                fluidRow(
                                  column(6, withSpinner(plotOutput("umap_integrated_celltype_soft"))),
                                  column(6, withSpinner(plotOutput("umap_integrated_anonymous_soft")))
                                )
                       ),
                       tabPanel("Medium Integration (theta=0.5)", 
                                fluidRow(
                                  column(6, withSpinner(plotOutput("umap_integrated_celltype_medium"))),
                                  column(6, withSpinner(plotOutput("umap_integrated_anonymous_medium")))
                                )
                       ),
                       tabPanel("Strong Integration (theta=5)", 
                                fluidRow(
                                  column(6, withSpinner(plotOutput("umap_integrated_celltype_strong"))),
                                  column(6, withSpinner(plotOutput("umap_integrated_anonymous_strong")))
                                )
                       )
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "output.umap_integrated_celltype_soft",
                   fluidRow(
                     column(12, align="center", uiOutput("guess_feedback_ui"))
                   ),
                   hr(),
                   fluidRow(
                     column(12, align = "center",
                            h4("Which cell type seems to be the most difficult to integrate?"),
                            p("Look closely at the 'Soft Integration' plot. One cell type still seems somewhat separated into two groups. This indicates underlying biological complexity."),
                            actionButton("act2_q3_tcell", "T-Cell"),
                            actionButton("act2_q3_bcell", "B-Cell"),
                            actionButton("act2_q3_mac", "Macrophage"),
                            actionButton("act2_q3_dc", "Dendritic-Cell"),
                            uiOutput("act2_q3_feedback")
                     )
                   ),
                   conditionalPanel(
                     condition = "output.act2_q3_correct == true",
                     hr(),
                     fluidRow(
                       column(12, align="center",
                              div(class="conclusion-box",
                                  h3("Act 2 Conclusion"),
                                  p("We've faced the critical challenge of batch effects and conquered it with data integration. We've also seen how tuning integration parameters is crucial and that some cell types, due to their own internal diversity, are harder to integrate than others."),
                                  p(strong("But the battle isn't over! A third wave of mysterious cells has just arrived on the scene, and we need to figure out who they are.")),
                                  actionButton("goToAct3", "Begin Act 3: The Reinforcements", class="btn-info")
                              )
                       )
                     )
                   )
                 )
        ),
        
        tabPanel("Act 3: The Reinforcements",
                 fluidRow(column(12, h2("Mysterious Reinforcements Arrive!", align="center"), p("The battle is tough! A third wave of cells (Batch 3) arrives from a different clinic to help. Their identities are unknown, but we have a powerful tool: our integrated map from Act 2. We can use it as a reference to classify these newcomers. This is 'label transfer'—projecting new, unlabeled data onto an existing, well-defined reference.", align="center"))),
                 hr(),
                 
                 conditionalPanel(
                   condition = "output.game_is_active == true",
                   fluidRow(
                     column(6, h4("Reference Cell (from our Atlas)", align = "center"), withSpinner(plotOutput("ref_cell_transfer"))),
                     column(6, h4("Query Cell (New Arrival)", align = "center"), withSpinner(plotOutput("query_cell_transfer")))
                   ),
                   hr(),
                   fluidRow(
                     column(12, align="center",
                            uiOutput("game_status_text"),
                            p("Based on the reference, what cell type is the new arrival?"),
                            actionButton("vote_t", "T-Cell", width="20%"),
                            actionButton("vote_b", "B-Cell", width="20%"),
                            actionButton("vote_m", "Macrophage", width="20%"),
                            actionButton("vote_dc", "Dendritic-Cell", width="20%")
                     )
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "output.game_is_active == false",
                   fluidRow(
                     column(12, align="center",
                            h3("Great work, Commander! Your classifications have been recorded."),
                            p("You just performed manual label transfer. Now, let's see how a computer algorithm performs the same task on the entire dataset. It will try to find the best 'anchor' for each new cell within our reference map and transfer the corresponding label."),
                            actionButton("run_label_transfer", "Run Label Transfer & Compare Results!", class="btn-danger btn-lg")
                     )
                   )
                 ),
                 
                 hr(),
                 
                 conditionalPanel(
                   condition = "input.run_label_transfer > 0",
                   fluidRow(
                     column(6,
                            h4("Your Accuracy vs. Algorithm", align="center"),
                            withSpinner(plotOutput("accuracy_comparison_plot"))
                     ),
                     column(6,
                            h4("Algorithm's Detailed Performance", align="center"),
                            withSpinner(plotOutput("label_transfer_results"))
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = "output.accuracy_comparison_plot",
                   hr(),
                   fluidRow(
                     column(12, align="center",
                            div(class="conclusion-box",
                                h3("Act 3 Conclusion"),
                                p("Label transfer is a powerful and essential technique for analyzing new single-cell data quickly and consistently. By leveraging a well-curated reference atlas, we can rapidly annotate new cells."),
                                p(strong("But what if we had no labels to begin with? In the final act, a disaster strikes, forcing us to discover the cell types from scratch.")),
                                actionButton("goToAct4", "Begin Act 4: The Lost Labels", class="btn-info")
                            )
                     )
                   )
                 )
        ),
        
        tabPanel("Act 4: The Lost Labels",
                 # Top-level check: Has Act 2 been completed?
                 conditionalPanel(
                   condition = "output.act2_integrations_complete == true",
                   
                   # --- STEP 1: Introduction and Cluster Button ---
                   fluidRow(
                     column(12, 
                            h2("The Bookkeeper's Dilemma", align="center"), 
                            p("Disaster! After all that work, the bookkeeper dropped the box of labels. Now all the cells from our beautifully integrated atlas are mixed up. All we know is that there should be four main cell types. This is a common scenario in research—analyzing a dataset to discover its contents without prior knowledge. We must perform 'unsupervised clustering' to group the cells based only on their features. We will use the data from our 'Soft Integration', as it seemed to preserve the most biological detail.", align="center")
                     )
                   ),
                   hr(),
                   fluidRow(
                     column(12, align="center",
                            actionButton("cluster_button", "Cluster the Cells!", class="btn-primary btn-lg")
                     )
                   ),
                   
                   # --- STEP 2: Show clustering results ---
                   conditionalPanel(
                     condition = "input.cluster_button > 0",
                     hr(),
                     fluidRow(column(12, h3("A Surprising Result", align="center"), p("The clustering algorithm finished, but it found 5 clusters, not 4! This is a fascinating result. Often, computational tools can see subtle patterns that we miss. Let's visualize the cells from each cluster to figure out what's going on."))),
                     fluidRow(
                       column(8, withSpinner(plotOutput("act4_umap_clusters"))),
                       column(4, 
                              h4("Analysis Log"),
                              verbatimTextOutput("clustering_console_output")
                       )
                     ),
                     hr(),
                     fluidRow(column(12, align="center", actionButton("visualize_clusters_button", "Visualize Cells from Each Cluster", class="btn-primary btn-lg"))),
                     
                     # --- STEP 3: Show cluster annotation UI ---
                     conditionalPanel(
                       condition = "input.visualize_clusters_button > 0",
                       hr(),
                       h3("Annotate the Clusters", align="center"),
                       p("Based on the cell comics below, assign a cell type to each cluster. You are told that two of the clusters represent T-Cells. This process is called 'cluster annotation', a crucial step where biologists use their knowledge to label computationally-defined groups.", align="center"),
                       lapply(0:4, function(i) {
                         fluidRow(
                           column(6, withSpinner(plotOutput(paste0("act4_cluster_", i, "_examples"), height="150px"))),
                           column(6, selectInput(paste0("act4_assign_cluster_", i), h5(paste("Assign Label to Cluster", i)), choices=c("", "T-Cell", "B-Cell", "Macrophage", "Dendritic-Cell")))
                         )
                       }),
                       hr(),
                       fluidRow(column(12, align="center", actionButton("check_assignments_button", "Check My Assignments!", class="btn-success btn-lg"))),
                       
                       # --- STEP 4: Show annotation results ---
                       conditionalPanel(
                         condition = "input.check_assignments_button > 0",
                         hr(),
                         fluidRow(
                           column(12, 
                                  h3("The Moment of Truth", align="center"), 
                                  p("Just in time, the bookkeeper found the original box of labels! Let's see how your assignments compare to the ground truth.")
                           )
                         ),
                         fluidRow(plotOutput("act4_assignment_confusion")),
                         hr(),
                         fluidRow(
                           column(12, align="center",
                                  h3("The T-Cell Mystery Revealed", align="center"),
                                  p("Your annotation was likely very accurate! But notice how the T-Cells are split into two separate clusters. One of their features must be consistently different between the two groups."),
                                  hr(),
                                  fluidRow(
                                    column(6, h5("T-Cell Subgroup 1", align="center"), withSpinner(plotOutput("act4_tcell_cluster_A_examples", height="150px"))),
                                    column(6, h5("T-Cell Subgroup 2", align="center"), withSpinner(plotOutput("act4_tcell_cluster_B_examples", height="150px")))
                                  ),
                                  hr(),
                                  h4("Which feature do you think is responsible for separating the T-Cells?"),
                                  actionButton("act4_guess_mouth", "Mouth Expression"),
                                  actionButton("act4_guess_moustache", "Moustache Size"),
                                  actionButton("act4_guess_hair", "Hair Style"),
                                  actionButton("act4_guess_color", "Cell Color"),
                                  uiOutput("act4_q1_feedback")
                           )
                         ),
                         
                         # --- STEP 5: Ask about biological vs batch ---
                         conditionalPanel(
                           condition = "output.act4_q1_is_correct == true",
                           hr(),
                           fluidRow(
                             column(12, align="center",
                                    h4("Is this difference a 'batch effect' or a real 'biological effect'?", align="center"),
                                    p("Think back to the 'rollerblades' in Act 2. That was a technical artifact affecting an entire batch. Is 'mouth expression' similar, or is it a consistent feature within the T-cell population regardless of batch?"),
                                    actionButton("tcell_difference_bio", "It's a Biological Effect", class="btn-info"),
                                    actionButton("tcell_difference_batch", "It's a Batch Effect", class="btn-warning")
                             )
                           ),
                           
                           # --- STEP 6: Show final plots and over-integration question ---
                           conditionalPanel(
                             condition = "input.tcell_difference_bio > 0 || input.tcell_difference_batch > 0",
                             hr(),
                             fluidRow(column(12, uiOutput("act4_tcell_feedback"))),
                             fluidRow(
                               column(6, plotOutput("act4_umap_subtypes")),
                               column(6, plotOutput("act4_subtype_confusion"))
                             ),
                             hr(),
                             fluidRow(
                               column(6,
                                      h4("Could we have discovered these subtypes with 'Strong' integration?", align="center"),
                                      p("The UMAP on the right shows the true subtypes on the 'Strong' integration space. Notice how they are completely mixed."),
                                      actionButton("act4_q2_no", "No"),
                                      actionButton("act4_q2_yes", "Yes"),
                                      uiOutput("act4_q2_feedback")
                               ),
                               column(6,
                                      withSpinner(plotOutput("strong_integration_subtypes_plot"))
                               )
                             ),
                             
                             # --- STEP 7: Show over-integration proof and conclusion ---
                             conditionalPanel(
                               condition = "input.act4_q2_no > 0",
                               hr(),
                               fluidRow(
                                 column(12, 
                                        h3("The Dangers of Over-integration", align="center"),
                                        p("You are absolutely right. If we had run the same clustering workflow on the 'Strong' integration data, it would have only found 4 clusters, completely missing the T-cell subtype distinction. The plot below confirms this.", align="center"),
                                        p(strong("This is a critical lesson:"), " over-integration can be as bad as under-integration. By trying too hard to merge batches, we can accidentally erase real, subtle biological differences that we want to discover. Finding the right balance is a key skill in single-cell analysis.", align="center")
                                 )
                               ),
                               fluidRow(
                                 column(12, align="center", withSpinner(plotOutput("strong_integration_clusters_plot")))
                               ),
                               hr(),
                               fluidRow(
                                 column(12, align="center",
                                        div(class="conclusion-box",
                                            h3("Act 4 & Story Conclusion"),
                                            p("Congratulations! You have completed the scRNA Integration Story. You started with a simple, clean dataset, confronted and solved a critical batch effect, used your integrated atlas to identify unknown cells, and finally, performed an unbiased analysis to discover novel cell subtypes hidden within the data."),
                                            p(strong("This journey from a messy, multi-batch dataset to the discovery of new biology represents the core challenge and ultimate triumph of single-cell analysis. You're now ready to face your own data adventures!")),
                                        )
                                 )
                               )
                             ) # end STEP 7
                           ) # end STEP 6
                         ) # end STEP 5
                       ) # end STEP 4
                     ) # end STEP 3
                   ) # end STEP 2
                 ), # end Top-level check
                 
                 # Message if Act 2 is not complete
                 conditionalPanel(
                   condition = "output.act2_integrations_complete == false",
                   fluidRow(
                     column(12, align="center",
                            h2("Awaiting Prerequisite Data"),
                            p("The analysis in Act 4 depends on the integrated data created in Act 2."),
                            p(strong("Please go back to Act 2, complete all integration steps, and then return here.")),
                            actionButton("backToAct2", "Go to Act 2", class="btn-warning")
                     )
                   )
                 )
        ) # end tabPanel Act 4
      ) # end tabsetPanel
    }
  })
  
  # ============================================================================
  # SERVER LOGIC
  # ============================================================================
  
  observeEvent(input$start_game, {
    req(input$seed)
    if (input$game_mode == "Live (on-the-fly calculations)") {
      withProgress(message = 'Generating base data...', value = 0.5, {
        set.seed(input$seed)
        sub_cell_types_b1 <- c("Helper T-Cell", "B-Cell", "Macrophage", "Dendritic-Cell")
        sub_cell_types_b2 <- c("Cytotoxic T-Cell", "B-Cell", "Macrophage", "Dendritic-Cell")
        sub_cell_types_b3 <- c("Helper T-Cell", "Cytotoxic T-Cell", "B-Cell", "Macrophage", "Dendritic-Cell")
        n_per_type <- 1000
        rv$b1_data <- bind_rows(lapply(sub_cell_types_b1, function(sct) generate_cell_data(n_per_type, sct, "Batch_1")))
        rv$b2_data <- bind_rows(lapply(sub_cell_types_b2, function(sct) generate_cell_data(n_per_type, sct, "Batch_2")))
        rv$b3_data <- bind_rows(lapply(sub_cell_types_b3, function(sct) generate_cell_data(n_per_type, sct, "Batch_3")))
        setProgress(1, detail = "Ready!")
      })
    } else {
      cache_dir <- "cache"
      cache_file <- file.path(cache_dir, paste0("singleCellComic_cache_seed_", input$seed, ".rds"))
      if (file.exists(cache_file)) {
        withProgress(message = 'Loading cached data...', value = 0.5, {
          cached_data <- readRDS(cache_file)
          for(name in names(cached_data)) { rv[[name]] <- cached_data[[name]] }
          setProgress(1, detail = "Done!")
        })
      } else {
        withProgress(message = 'Running initial analysis (one-time for this seed)...', value = 0, {
          set.seed(input$seed)
          setProgress(0.05, detail = "Generating cell populations...")
          sub_cell_types_b1 <- c("Helper T-Cell", "B-Cell", "Macrophage", "Dendritic-Cell")
          sub_cell_types_b2 <- c("Cytotoxic T-Cell", "B-Cell", "Macrophage", "Dendritic-Cell")
          sub_cell_types_b3 <- c("Helper T-Cell", "Cytotoxic T-Cell", "B-Cell", "Macrophage", "Dendritic-Cell")
          n_per_type <- 1000
          temp_b1_data <- bind_rows(lapply(sub_cell_types_b1, function(sct) generate_cell_data(n_per_type, sct, "Batch_1")))
          temp_b2_data <- bind_rows(lapply(sub_cell_types_b2, function(sct) generate_cell_data(n_per_type, sct, "Batch_2")))
          temp_b3_data <- bind_rows(lapply(sub_cell_types_b3, function(sct) generate_cell_data(n_per_type, sct, "Batch_3")))
          setProgress(0.20, detail = "Analyzing Act 1...")
          b1_counts <- t(as.matrix(select(temp_b1_data, -cell_type, -batch, -sub_cell_type)))
          b1_metadata <- select(temp_b1_data, cell_type, batch, sub_cell_type)
          temp_act1_seurat <- CreateSeuratObject(counts = b1_counts, meta.data = b1_metadata) %>% NormalizeData(verbose = FALSE) %>% ScaleData(features = rownames(.), verbose = FALSE) %>% RunPCA(features = rownames(.), npcs = 10, svd.method = 'svd', verbose = FALSE) %>% RunUMAP(dims = 1:10, verbose = FALSE)
          setProgress(0.35, detail = "Analyzing Act 2 (Unintegrated)...")
          combined_data <- rbind(temp_b1_data, temp_b2_data)
          counts <- t(as.matrix(select(combined_data, -cell_type, -batch, -sub_cell_type)))
          metadata <- select(combined_data, cell_type, batch, sub_cell_type)
          temp_unintegrated_seurat <- CreateSeuratObject(counts = counts, meta.data = metadata) %>% NormalizeData(verbose = FALSE) %>% ScaleData(features = rownames(.), verbose = FALSE) %>% RunPCA(features = rownames(.), npcs = 10, svd.method = 'svd', verbose = FALSE) %>% RunUMAP(dims = 1:10, verbose = FALSE)
          temp_unintegrated_seurat$batch_celltype <- paste(temp_unintegrated_seurat$batch, temp_unintegrated_seurat$cell_type, sep="_")
          cell_types <- sort(unique(temp_unintegrated_seurat$cell_type))
          n_types <- length(cell_types)
          b1_anon_labels <- sample(paste("Population", LETTERS[1:n_types]))
          b2_anon_labels <- sample(paste("Population", LETTERS[(n_types+1):(2*n_types)]))
          b1_true_labels <- paste("Batch_1", cell_types, sep="_"); b2_true_labels <- paste("Batch_2", cell_types, sep="_")
          map1 <- setNames(b1_anon_labels, b1_true_labels); map2 <- setNames(b2_anon_labels, b2_true_labels)
          temp_label_map <- c(map1, map2)
          temp_unintegrated_seurat$anonymous_labels <- unname(temp_label_map[temp_unintegrated_seurat$batch_celltype])
          setProgress(0.50, detail = "Analyzing Act 2 (Soft Integration)...")
          temp_integrated_seurat_soft <- RunHarmony(temp_unintegrated_seurat, group.by.vars = "batch", reduction.use = "pca", dims.use = 1:10, verbose = FALSE, reduction.save="harmony_soft", theta = 0.05) %>% RunUMAP(reduction = "harmony_soft", dims = 1:10, verbose = FALSE, return.model = TRUE, reduction.name = "umap_soft", reduction.key = "UMAPsoft_")
          setProgress(0.60, detail = "Analyzing Act 2 (Medium Integration)...")
          temp_integrated_seurat_medium <- RunHarmony(temp_unintegrated_seurat, group.by.vars = "batch", reduction.use = "pca", dims.use = 1:10, verbose = FALSE, reduction.save="harmony_medium", theta = 0.5) %>% RunUMAP(reduction = "harmony_medium", dims = 1:10, verbose = FALSE, return.model = TRUE, reduction.name = "umap_medium", reduction.key = "UMAPmedium_")
          setProgress(0.70, detail = "Analyzing Act 2 (Strong Integration)...")
          temp_integrated_seurat_strong <- RunHarmony(temp_unintegrated_seurat, group.by.vars = "batch", reduction.use = "pca", dims.use = 1:10, verbose = FALSE, reduction.save="harmony_strong", theta = 5) %>% RunUMAP(reduction = "harmony_strong", dims = 1:10, verbose = FALSE, return.model = TRUE, reduction.name = "umap_strong", reduction.key = "UMAPstrong_")
          setProgress(0.80, detail = "Analyzing Act 3...")
          query_counts <- t(as.matrix(select(temp_b3_data, -cell_type, -batch, -sub_cell_type)))
          query_metadata <- select(temp_b3_data, batch)
          query.obj <- CreateSeuratObject(counts = query_counts, meta.data = query_metadata) %>% NormalizeData(verbose=FALSE)
          anchors <- FindTransferAnchors(reference = temp_integrated_seurat_soft, query = query.obj, dims = 1:10, features = rownames(temp_integrated_seurat_soft), reference.reduction = "harmony_soft")
          query.obj <- MapQuery(anchorset = anchors, reference = temp_integrated_seurat_soft, query = query.obj, refdata = list(celltype = "cell_type"), reference.reduction = "harmony_soft", reduction.model = "umap_soft")
          algo_correct <- sum(query.obj$predicted.celltype == temp_b3_data$cell_type)
          algo_accuracy <- algo_correct / nrow(temp_b3_data)
          predicted_labels <- factor(query.obj$predicted.celltype, levels = cell_types)
          ground_truth_labels <- factor(temp_b3_data$cell_type, levels = cell_types)
          temp_label_transfer_results <- list(algo_accuracy = algo_accuracy, confusion_df = as.data.frame(table(Predicted = predicted_labels, Truth = ground_truth_labels)))
          setProgress(0.90, detail = "Analyzing Act 4...")
          temp_clustered_seurat_soft <- FindNeighbors(temp_integrated_seurat_soft, reduction = "harmony_soft", dims = 1:10, verbose = FALSE) %>% FindClusters(resolution = 0.1, verbose = FALSE)
          temp_clustered_seurat_strong <- FindNeighbors(temp_integrated_seurat_strong, reduction = "harmony_strong", dims = 1:10, verbose = FALSE) %>% FindClusters(resolution = 0.1, verbose = FALSE)
          data_to_cache <- list(b1_data = temp_b1_data, b2_data = temp_b2_data, b3_data = temp_b3_data, act1_seurat = temp_act1_seurat, unintegrated_seurat = temp_unintegrated_seurat, integrated_seurat_soft = temp_integrated_seurat_soft, integrated_seurat_medium = temp_integrated_seurat_medium, integrated_seurat_strong = temp_integrated_seurat_strong, clustered_seurat_soft = temp_clustered_seurat_soft, clustered_seurat_strong = temp_clustered_seurat_strong, label_transfer_results = temp_label_transfer_results, label_map = temp_label_map, act1_log_complete = "## Initializing Seurat object...\n\n## Normalizing data...\n\n## Scaling data...\n\n## Running PCA...\n\n## Running UMAP...", clustering_log_complete = "## Building graph of cell similarity...\n\n## Identifying communities (clusters)...")
          if (!dir.exists(cache_dir)) dir.create(cache_dir)
          saveRDS(data_to_cache, file = cache_file)
          for(name in names(data_to_cache)) { rv[[name]] <- data_to_cache[[name]] }
          setProgress(1.0, detail = "Done!")
        })
      }
    }
    req(rv$b3_data)
    game_df <- rv$b3_data %>% group_by(cell_type) %>% sample_n(2) %>% ungroup() %>% sample_frac(1)
    rv$game_cells <- game_df
    rv$game_ready <- TRUE
  })
  
  observeEvent(input$run_act1_umap, { if (isTRUE(rv$act1_already_run)) return(); progress <- shiny::Progress$new(session, min=0, max=1); on.exit(progress$close()); if (isolate(input$game_mode) == "Caching (faster startup)") { progress$set(message = "Loading analysis...", value = 0); log_steps <- str_split(rv$act1_log_complete, "\n\n")[[1]]; current_log <- ""; for (i in seq_along(log_steps)) { current_log <- paste(current_log, log_steps[i], sep = "\n\n"); rv$act1_log_display <- trimws(current_log); progress$inc(1 / length(log_steps), detail = gsub("## ", "", log_steps[i])); Sys.sleep(1.2) } } else { progress$set(message = "Analyzing Batch 1...", value = 0); rv$act1_log_display <- ""; update_log <- function(new_text, prog_val, detail_text) { rv$act1_log_display <- trimws(paste(rv$act1_log_display, new_text, sep="\n\n")); progress$set(value = prog_val, detail = detail_text); Sys.sleep(0.5) }; req(rv$b1_data); update_log("## Initializing Seurat object...", 0.1, "Creating object"); b1_counts <- t(as.matrix(select(rv$b1_data, -cell_type, -batch, -sub_cell_type))); b1_metadata <- select(rv$b1_data, cell_type, batch, sub_cell_type); seurat_obj <- CreateSeuratObject(counts = b1_counts, meta.data = b1_metadata); update_log("## Normalizing data...", 0.3, "Normalizing"); seurat_obj <- NormalizeData(seurat_obj, verbose = FALSE); update_log("## Scaling data...", 0.5, "Scaling"); seurat_obj <- ScaleData(seurat_obj, features = rownames(seurat_obj), verbose = FALSE); update_log("## Running PCA...", 0.7, "Running PCA"); seurat_obj <- RunPCA(seurat_obj, features = rownames(seurat_obj), npcs = 10, svd.method = 'svd', verbose = FALSE); update_log("## Running UMAP...", 0.9, "Running UMAP"); seurat_obj <- RunUMAP(seurat_obj, dims = 1:10, verbose = FALSE); rv$act1_seurat <- seurat_obj }; rv$act1_already_run <- TRUE })
  observeEvent(input$run_unintegrated, { if(isTRUE(rv$unintegrated_already_run)) return(); withProgress(message="Combining & Analyzing Data...", { if(!is.null(rv$unintegrated_seurat)) { Sys.sleep(1); return() }; req(rv$b1_data, rv$b2_data); combined_data <- rbind(rv$b1_data, rv$b2_data); counts <- t(as.matrix(select(combined_data, -cell_type, -batch, -sub_cell_type))); metadata <- select(combined_data, cell_type, batch, sub_cell_type); seurat_obj <- CreateSeuratObject(counts = counts, meta.data = metadata) %>% NormalizeData(verbose = FALSE) %>% ScaleData(features = rownames(.), verbose = FALSE) %>% RunPCA(features = rownames(.), npcs = 10, svd.method = 'svd', verbose = FALSE) %>% RunUMAP(dims = 1:10, verbose = FALSE); seurat_obj$batch_celltype <- paste(seurat_obj$batch, seurat_obj$cell_type, sep="_"); cell_types <- sort(unique(seurat_obj$cell_type)); n_types <- length(cell_types); b1_anon_labels <- sample(paste("Population", LETTERS[1:n_types])); b2_anon_labels <- sample(paste("Population", LETTERS[(n_types+1):(2*n_types)])); b1_true_labels <- paste("Batch_1", cell_types, sep="_"); b2_true_labels <- paste("Batch_2", cell_types, sep="_"); map1 <- setNames(b1_anon_labels, b1_true_labels); map2 <- setNames(b2_anon_labels, b2_true_labels); rv$label_map <- c(map1, map2); seurat_obj$anonymous_labels <- unname(rv$label_map[seurat_obj$batch_celltype]); rv$unintegrated_seurat <- seurat_obj }); rv$unintegrated_already_run <- TRUE })
  observeEvent(input$run_integrated, { if(isTRUE(rv$integrated_already_run)) return(); progress <- shiny::Progress$new(session, min=0, max=1); on.exit(progress$close()); if(isolate(input$game_mode) == "Caching (faster startup)") { progress$set(message="Loading Integrations...", value=0.5); Sys.sleep(2); progress$set(1) } else { progress$set(message="Integrating with Harmony...", value=0); req(rv$unintegrated_seurat); progress$set(0.1, detail="Theta = 0.05 (Soft)"); rv$integrated_seurat_soft <- RunHarmony(rv$unintegrated_seurat, group.by.vars = "batch", reduction.use = "pca", dims.use = 1:10, verbose = FALSE, reduction.save="harmony_soft", theta = 0.05) %>% RunUMAP(reduction = "harmony_soft", dims = 1:10, verbose = FALSE, return.model = TRUE, reduction.name = "umap_soft", reduction.key = "UMAPsoft_"); progress$set(0.4, detail="Theta = 0.5 (Medium)"); rv$integrated_seurat_medium <- RunHarmony(rv$unintegrated_seurat, group.by.vars = "batch", reduction.use = "pca", dims.use = 1:10, verbose = FALSE, reduction.save="harmony_medium", theta = 0.5) %>% RunUMAP(reduction = "harmony_medium", dims = 1:10, verbose = FALSE, return.model = TRUE, reduction.name = "umap_medium", reduction.key = "UMAPmedium_"); progress$set(0.7, detail="Theta = 5 (Strong)"); rv$integrated_seurat_strong <- RunHarmony(rv$unintegrated_seurat, group.by.vars = "batch", reduction.use = "pca", dims.use = 1:10, verbose = FALSE, reduction.save="harmony_strong", theta = 5) %>% RunUMAP(reduction = "harmony_strong", dims = 1:10, verbose = FALSE, return.model = TRUE, reduction.name = "umap_strong", reduction.key = "UMAPstrong_"); progress$set(1, detail="Done!") }; rv$integrated_already_run <- TRUE; rv$act2_integrations_complete <- TRUE })
  observeEvent(input$cluster_button, { if (isTRUE(rv$clustered_already_run)) return(); progress <- shiny::Progress$new(session, min=0, max=1); on.exit(progress$close()); if (isolate(input$game_mode) == "Caching (faster startup)") { progress$set(message = "Loading analysis...", value = 0); log_steps <- str_split(rv$clustering_log_complete, "\n\n")[[1]]; current_log <- ""; for (i in seq_along(log_steps)) { current_log <- paste(current_log, log_steps[i], sep = "\n\n"); rv$clustering_log_display <- trimws(current_log); progress$inc(1 / length(log_steps), detail = gsub("## ", "", log_steps[i])); Sys.sleep(1.2) } } else { progress$set(message = "Clustering cells...", value = 0); rv$clustering_log_display <- ""; req(rv$integrated_seurat_soft); rv$clustering_log_display <- "## Building graph of cell similarity..."; progress$set(value = 0.3, detail = "Finding Neighbors"); Sys.sleep(0.5); seurat_obj <- FindNeighbors(rv$integrated_seurat_soft, reduction = "harmony_soft", dims = 1:10, verbose = FALSE); rv$clustering_log_display <- paste(rv$clustering_log_display, "\n\n## Identifying communities (clusters)..."); progress$set(value = 0.8, detail = "Finding Clusters"); Sys.sleep(0.5); seurat_obj <- FindClusters(seurat_obj, resolution = 0.1, verbose = FALSE); rv$clustered_seurat_soft <- seurat_obj; seurat_obj_strong <- FindNeighbors(rv$integrated_seurat_strong, reduction = "harmony_strong", dims = 1:10, verbose = FALSE);seurat_obj_strong <- FindClusters(seurat_obj_strong, resolution = 0.1, verbose = FALSE); rv$clustered_seurat_strong <- seurat_obj_strong}; rv$clustered_already_run <- TRUE })
  
  observeEvent(input$goToAct2, {
    updateTabsetPanel(session, "main_tabs", selected = "Act 2: The Viral Invasion")
  })
  
  observeEvent(input$goToAct3, {
    updateTabsetPanel(session, "main_tabs", selected = "Act 3: The Reinforcements")
  })
  
  observeEvent(input$goToAct4, {
    updateTabsetPanel(session, "main_tabs", selected = "Act 4: The Lost Labels")
  })
  
  observeEvent(input$backToAct2, {
    updateTabsetPanel(session, "main_tabs", selected = "Act 2: The Viral Invasion")
  })
  lapply(c("T-Cell", "B-Cell", "Macrophage", "Dendritic-Cell"), function(ct) { output[[paste0(ct, "_intro_plot")]] <- renderPlot({ req(rv$b1_data); example_cells <- filter(rv$b1_data, cell_type == ct) %>% sample_n(10); plot_list <- pmap(select(example_cells, -cell_type, -batch, -sub_cell_type), plotCellComic); wrap_plots(plot_list, nrow = 1) }) })
  output$act1_example_cells <- renderPlot({ req(rv$b1_data); example_cells <- sample_n(rv$b1_data, 8); plot_list <- pmap(select(example_cells, -cell_type, -batch, -sub_cell_type), plotCellComic); wrap_plots(plot_list, nrow = 1) })
  output$act1_umap_celltype <- renderPlot({ req(rv$act1_seurat); DimPlot(rv$act1_seurat, group.by = "cell_type") + labs(title = "Populations by Main Cell Type") })
  output$act1_console_output <- renderText({ rv$act1_log_display })
  output$act1_feature_plot <- renderPlot({ req(rv$act1_seurat); FeaturePlot(rv$act1_seurat, features = input$act1_feature_select) + labs(title = paste("Gene Expression on UMAP")) })
  output$act1_vln_plot <- renderPlot({ req(rv$act1_seurat); VlnPlot(rv$act1_seurat, features = input$act1_feature_select, group.by = "cell_type") + NoLegend() + labs(title = paste("Gene Expression Distribution")) })
  output$prop_mapping_plot <- renderPlot({ p1 <- plotCellComic(moustache.size = 150) + labs(subtitle = "Expression: 150"); p2 <- plotCellComic(moustache.size = 500) + labs(subtitle = "Expression: 500"); p3 <- plotCellComic(moustache.size = 950) + labs(subtitle = "Expression: 950"); wrap_plots(p1, p2, p3, nrow = 1) })
  output$cat_mapping_plot <- renderPlot({ p1 <- plotCellComic(hairstyle.value = 250) + labs(subtitle = "Expr: 250 (Spiky)"); p2 <- plotCellComic(hairstyle.value = 550) + labs(subtitle = "Expr: 550 (Afro)"); p3 <- plotCellComic(hairstyle.value = 850) + labs(subtitle = "Expr: 850 (Flowing)"); wrap_plots(p1, p2, p3, nrow = 1) })
  output$batch1_example <- renderPlot({ req(rv$b1_data); example_cells <- sample_n(rv$b1_data, 10); plot_list <- pmap(select(example_cells, -cell_type, -batch, -sub_cell_type), plotCellComic); wrap_plots(plot_list, nrow = 2) + plot_annotation(subtitle = "A sample of the diverse blood cells") })
  output$batch2_example <- renderPlot({ req(rv$b2_data); example_cells <- sample_n(rv$b2_data, 10); plot_list <- pmap(select(example_cells, -cell_type, -batch, -sub_cell_type), plotCellComic); wrap_plots(plot_list, nrow = 2) + plot_annotation(subtitle = "A sample of the tissue cells") })
  
  observeEvent(input$act2_guess_rollerblades, { req(input$act2_guess_rollerblades > 0); rv$act2_q1_correct <- TRUE; rv$act2_q1_feedback <- tags$div(style="margin-top:15px; padding: 10px; background-color: #e8f5e9; border-radius: 5px;", h5(style="color: green; font-weight:bold;", "Correct!"), p("The cells in Batch 2 (from the tissue) almost all have a strong 'rollerblade' feature that is absent in Batch 1 (from the blood).")) }, ignoreInit = TRUE)
  set_wrong_feedback <- function() { rv$act2_q1_correct <- FALSE; rv$act2_q1_feedback <- tags$div(style="margin-top:15px; padding: 10px; background-color: #ffebee; border-radius: 5px;", h5(style="color: red; font-weight:bold;", "Not quite!"), p("While other features might vary slightly, the most consistent and dramatic difference is that nearly all cells in Batch 2 have rollerblades. Take another look at the examples above.")) }
  observeEvent(input$act2_guess_moustache, { req(input$act2_guess_moustache > 0); set_wrong_feedback() }, ignoreInit = TRUE)
  observeEvent(input$act2_guess_headphones, { req(input$act2_guess_headphones > 0); set_wrong_feedback() }, ignoreInit = TRUE)
  observeEvent(input$act2_guess_hair, { req(input$act2_guess_hair > 0); set_wrong_feedback() }, ignoreInit = TRUE)
  output$act2_q1_feedback <- renderUI({ rv$act2_q1_feedback })
  output$act2_q1_correct <- reactive({ rv$act2_q1_correct }); outputOptions(output, "act2_q1_correct", suspendWhenHidden = FALSE)
  observeEvent(input$act2_guess_batch, { req(input$act2_guess_batch > 0); rv$act2_q2_correct <- TRUE; rv$act2_q2_feedback <- tags$div(style="margin-top:15px; padding: 10px; background-color: #e8f5e9; border-radius: 5px;", h5(style="color: green; font-weight:bold;", "Correct!"), p("Because this feature is almost exclusively present in one batch and not the other, it's very likely a technical artifact. These are called 'batch effects', and they can hide the true biological similarities between cell types if not handled correctly.")) }, ignoreInit = TRUE)
  observeEvent(input$act2_guess_biological, { req(input$act2_guess_biological > 0); rv$act2_q2_correct <- FALSE; rv$act2_q2_feedback <- tags$div(style="margin-top:15px; padding: 10px; background-color: #ffebee; border-radius: 5px;", h5(style="color: red; font-weight:bold;", "A reasonable thought, but incorrect in this case."), p("If it were purely biological, you might expect to see at least *some* cells with rollerblades in Batch 1, or for the feature to be associated with a specific cell type rather than the entire batch. When a strong feature cleanly separates experimental batches like this, we should always be suspicious of technical artifacts.")) }, ignoreInit = TRUE)
  output$act2_q2_feedback <- renderUI({ rv$act2_q2_feedback })
  output$act2_q2_correct <- reactive({ rv$act2_q2_correct }); outputOptions(output, "act2_q2_correct", suspendWhenHidden = FALSE)
  output$umap_unintegrated_anonymous <- renderPlot({ req(rv$unintegrated_seurat); DimPlot(rv$unintegrated_seurat, group.by="anonymous_labels", label = TRUE) + labs(title="Unintegrated Data with Anonymous Labels") })
  output$umap_integrated_celltype_soft <- renderPlot({ req(rv$integrated_seurat_soft); DimPlot(rv$integrated_seurat_soft, group.by = "cell_type", reduction = "umap_soft") + labs(title="Integrated Data by True Cell Type") })
  output$umap_integrated_anonymous_soft <- renderPlot({ req(rv$integrated_seurat_soft); DimPlot(rv$integrated_seurat_soft, group.by="anonymous_labels", reduction = "umap_soft") + labs(title="Integrated Data with Anonymous Labels") })
  output$umap_integrated_celltype_medium <- renderPlot({ req(rv$integrated_seurat_medium); DimPlot(rv$integrated_seurat_medium, group.by = "cell_type", reduction = "umap_medium") + labs(title="Integrated Data by True Cell Type") })
  output$umap_integrated_anonymous_medium <- renderPlot({ req(rv$integrated_seurat_medium); DimPlot(rv$integrated_seurat_medium, group.by="anonymous_labels", reduction = "umap_medium") + labs(title="Integrated Data with Anonymous Labels") })
  output$umap_integrated_celltype_strong <- renderPlot({ req(rv$integrated_seurat_strong); DimPlot(rv$integrated_seurat_strong, group.by = "cell_type", reduction = "umap_strong") + labs(title="Integrated Data by True Cell Type") })
  output$umap_integrated_anonymous_strong <- renderPlot({ req(rv$integrated_seurat_strong); DimPlot(rv$integrated_seurat_strong, group.by="anonymous_labels", reduction = "umap_strong") + labs(title="Integrated Data with Anonymous Labels") })
  
  observeEvent(input$act2_q3_tcell, { rv$act2_q3_correct <- TRUE; rv$act2_q3_feedback <- tags$div(style="margin-top:15px; padding: 10px; background-color: #e8f5e9; border-radius: 5px;", h5(style="color: green; font-weight:bold;", "Correct!"), p("The T-cells are the hardest to merge. Think about why this could happen. "))})
  set_act2_q3_wrong_feedback <- function() { rv$act2_q3_correct <- FALSE; rv$act2_q3_feedback <- tags$div(style="margin-top:15px; padding: 10px; background-color: #ffebee; border-radius: 5px;", h5(style="color: red; font-weight:bold;", "Not quite!"), p("Look again at the 'Soft Integration' plot, colored by true cell type. Most of the populations merge quite nicely, but one of them still form two semi-distinct groups. Which one is it?")) }
  observeEvent(input$act2_q3_bcell, { set_act2_q3_wrong_feedback() })
  observeEvent(input$act2_q3_mac, { set_act2_q3_wrong_feedback() })
  observeEvent(input$act2_q3_dc, { set_act2_q3_wrong_feedback() })
  output$act2_q3_feedback <- renderUI({ rv$act2_q3_feedback })
  output$act2_q3_correct <- reactive({ rv$act2_q3_correct }); outputOptions(output, "act2_q3_correct", suspendWhenHidden = FALSE)
  
  output$matching_game_ui <- renderUI({ req(rv$label_map); b1_labels <- rv$label_map[grepl("Batch_1", names(rv$label_map))]; b2_labels <- rv$label_map[grepl("Batch_2", names(rv$label_map))]; lapply(sort(b1_labels), function(b1_label) { selectInput(inputId = paste0("match_", b1_label), label = paste(b1_label, "matches:"), choices = c("Select...", unname(sort(b2_labels)))) }) })
  observeEvent(input$lock_guesses, { req(rv$label_map); b1_labels <- rv$label_map[grepl("Batch_1", names(rv$label_map))]; correct_matches <- 0; total_matches <- length(b1_labels); feedback_text <- ""; for(b1_label in sort(b1_labels)) { user_guess_b2_label <- input[[paste0("match_", b1_label)]]; true_b1_name <- names(rv$label_map)[rv$label_map == b1_label]; true_cell_type <- gsub("Batch_1_", "", true_b1_name); true_b2_name <- names(rv$label_map)[rv$label_map == user_guess_b2_label]; guessed_cell_type <- gsub("Batch_2_", "", true_b2_name); if (length(guessed_cell_type) > 0 && true_cell_type == guessed_cell_type) { correct_matches <- correct_matches + 1; feedback_text <- paste0(feedback_text, "<li><span style='color:green;'>✔ Correct:</span> Your match for <strong>", true_cell_type, "s</strong> was right!</li>") } else { feedback_text <- paste0(feedback_text, "<li><span style='color:red;'>✘ Incorrect:</span> Your match for <strong>", true_cell_type, "s</strong> was wrong.</li>") } }; final_text <- paste0("<h4>Your Matching Results</h4>", "<p>You correctly identified <strong>", correct_matches, " out of ", total_matches, "</strong> cell type pairs.</p>", "<ul>", feedback_text, "</ul>", "<p>As you can see, batch effects make it incredibly difficult to be sure about cell identities. This is why integration is not just a nice-to-have, but an absolutely essential step in the analysis.</p>"); rv$guess_feedback <- HTML(final_text) })
  output$guess_feedback_ui <- renderUI({ rv$guess_feedback })
  output$game_is_active <- reactive({ !rv$game_finished }); outputOptions(output, "game_is_active", suspendWhenHidden = FALSE)
  output$game_status_text <- renderUI({ req(rv$game_cells); h4(paste("Classifying cell", rv$current_game_cell_idx, "of", nrow(rv$game_cells))) })
  output$query_cell_transfer <- renderPlot({ req(rv$game_cells); current_cell_data <- rv$game_cells[rv$current_game_cell_idx, ]; do.call(plotCellComic, as.list(select(current_cell_data, -cell_type, -batch, -sub_cell_type))) })
  output$ref_cell_transfer <- renderPlot({ req(rv$b1_data, rv$game_cells); current_cell_true_type <- rv$game_cells$cell_type[rv$current_game_cell_idx]; ref_cell_data <- rv$b1_data %>% filter(cell_type == current_cell_true_type) %>% sample_n(1); do.call(plotCellComic, as.list(select(ref_cell_data, -cell_type, -batch, -sub_cell_type))) })
  handle_vote <- function(guess) { rv$game_guesses <- c(rv$game_guesses, guess); if (rv$current_game_cell_idx < nrow(rv$game_cells)) { rv$current_game_cell_idx <- rv$current_game_cell_idx + 1 } else { rv$game_finished <- TRUE } }
  observeEvent(input$vote_t, { handle_vote("T-Cell") }); observeEvent(input$vote_b, { handle_vote("B-Cell") }); observeEvent(input$vote_m, { handle_vote("Macrophage") }); observeEvent(input$vote_dc, { handle_vote("Dendritic-Cell") })
  results_r <- eventReactive(input$run_label_transfer, { if (!is.null(rv$label_transfer_results)) { user_correct <- sum(rv$game_guesses == rv$game_cells$cell_type); user_accuracy <- user_correct / nrow(rv$game_cells); accuracy_df <- data.frame(Method = c("Your Accuracy", "Label Transfer"), Accuracy = c(user_accuracy, rv$label_transfer_results$algo_accuracy)); confusion_df <- rv$label_transfer_results$confusion_df } else { withProgress(message = 'Running Label Transfer...', value = 0, { req(rv$b3_data, rv$integrated_seurat_soft); setProgress(0.6, detail = "Mapping and predicting..."); query_counts <- t(as.matrix(select(rv$b3_data, -cell_type, -batch, -sub_cell_type))); query.obj <- CreateSeuratObject(counts = query_counts) %>% NormalizeData(verbose = FALSE); anchors <- FindTransferAnchors(reference = rv$integrated_seurat_soft, query = query.obj, dims = 1:10, features = rownames(rv$integrated_seurat_soft), reference.reduction = "harmony_soft"); query.obj <- MapQuery(anchorset = anchors, reference = rv$integrated_seurat_soft, query = query.obj, refdata = list(celltype = "cell_type"), reference.reduction = "harmony_soft", reduction.model = "umap_soft"); setProgress(0.9, detail = "Calculating accuracies..."); user_correct <- sum(rv$game_guesses == rv$game_cells$cell_type); user_accuracy <- user_correct / nrow(rv$game_cells); algo_correct <- sum(query.obj$predicted.celltype == rv$b3_data$cell_type); algo_accuracy <- algo_correct / nrow(rv$b3_data); predicted_labels <- query.obj$predicted.celltype; ground_truth_labels <- rv$b3_data$cell_type; all_levels <- unique(c(ground_truth_labels, predicted_labels)); confusion_df <- as.data.frame(table(Predicted = factor(predicted_labels, levels=all_levels), Truth = factor(ground_truth_labels, levels=all_levels))); accuracy_df <- data.frame(Method = c("Your Accuracy", "Label Transfer"), Accuracy = c(user_accuracy, algo_accuracy)); rv$label_transfer_results <- list(algo_accuracy = algo_accuracy, confusion_df = confusion_df) }) }; cm_plot <- ggplot(data = confusion_df, aes(x = Truth, y = Predicted, fill = Freq)) + geom_tile() + geom_text(aes(label = Freq), vjust = 0.5, size = 5) + scale_fill_gradient(low = "white", high = "#d9534f") + labs(x = "Ground Truth", y = "Predicted") + theme_minimal(base_size = 14) + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)); acc_plot <- ggplot(accuracy_df, aes(x = Method, y = Accuracy, fill = Method)) + geom_col(width=0.6) + geom_text(aes(label = scales::percent(Accuracy, accuracy = 0.1)), vjust = -0.5, size = 5) + scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) + scale_fill_manual(values = c("Your Accuracy" = "#5cb85c", "Label Transfer" = "#f0ad4e")) + labs(y = "Classification Accuracy") + theme_minimal(base_size = 14) + theme(legend.position = "none", axis.title.x = element_blank()); return(list(cm_plot = cm_plot, acc_plot = acc_plot)) })
  output$label_transfer_results <- renderPlot({ req(results_r()); results_r()$cm_plot })
  output$accuracy_comparison_plot <- renderPlot({ req(results_r()); results_r()$acc_plot })
  
  # Reactive output for conditionalPanel condition
  output$act2_integrations_complete <- reactive({
    rv$act2_integrations_complete
  })
  outputOptions(output, "act2_integrations_complete", suspendWhenHidden = FALSE)
  
  output$act4_umap_clusters <- renderPlot({ 
    req(input$cluster_button > 0, rv$clustered_seurat_soft)
    # Explicitly use the 'umap_soft' reduction for plotting
    DimPlot(rv$clustered_seurat_soft, group.by="seurat_clusters", label=TRUE, reduction = "umap_soft") + NoLegend() + labs(title="Unbiased Clustering finds 5 Groups") 
  })
  output$clustering_console_output <- renderText({ rv$clustering_log_display })
  lapply(0:4, function(i) { output[[paste0("act4_cluster_", i, "_examples")]] <- renderPlot({ req(rv$clustered_seurat_soft); seurat_obj <- rv$clustered_seurat_soft; cells_in_cluster_names <- rownames(seurat_obj@meta.data[seurat_obj@meta.data$seurat_clusters == as.character(i), ]); sampled_cell_names <- sample(cells_in_cluster_names, size = min(6, length(cells_in_cluster_names))); counts_subset <- seurat_obj[["RNA"]]$counts[, sampled_cell_names, drop = FALSE]; features_df <- as.data.frame(t(as.matrix(counts_subset))); if (nrow(features_df) > 0) { plot_list <- pmap(features_df, plotCellComic); wrap_plots(plot_list, nrow=1) } }) })
  assignment_cm_r <- eventReactive(input$check_assignments_button, { req(rv$clustered_seurat_soft); user_assignments <- tibble(seurat_clusters = as.factor(0:4), user_label = c(input$act4_assign_cluster_0, input$act4_assign_cluster_1, input$act4_assign_cluster_2, input$act4_assign_cluster_3, input$act4_assign_cluster_4)); truth_data <- rv$clustered_seurat_soft@meta.data %>% left_join(user_assignments, by = "seurat_clusters"); truth_data$user_label[truth_data$user_label == ""] <- "Unassigned"; truth_data <- truth_data %>% mutate(predicted_label_formatted = paste0("Cluster ", seurat_clusters, ": ", user_label)); confusion_df <- as.data.frame(table(Predicted = truth_data$predicted_label_formatted, Truth = truth_data$cell_type)); ggplot(data = confusion_df, aes(x = Truth, y = Predicted, fill = Freq)) + geom_tile() + geom_text(aes(label = Freq), vjust = 0.5, size = 5) + scale_fill_gradient(low = "white", high = "#5cb85c") + labs(title = "Your Assignments vs. Ground Truth", x = "Ground Truth", y = "Your Assignment") + theme_minimal(base_size=14) + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5, size = 16)) })
  output$act4_assignment_confusion <- renderPlot({ assignment_cm_r() })
  
  t_cell_clusters_r <- reactive({ req(rv$clustered_seurat_soft); rv$clustered_seurat_soft@meta.data %>% filter(cell_type == "T-Cell") %>% count(seurat_clusters, sort = TRUE) %>% top_n(2, n) %>% pull(seurat_clusters) %>% as.character() })
  
  plot_t_cell_cluster <- function(cluster_id) {
    req(rv$clustered_seurat_soft)
    seurat_obj <- rv$clustered_seurat_soft
    cells_in_cluster <- rownames(seurat_obj@meta.data[seurat_obj@meta.data$seurat_clusters == cluster_id, ])
    sampled_cells <- sample(cells_in_cluster, min(5, length(cells_in_cluster)))
    features_df <- as.data.frame(t(as.matrix(seurat_obj[["RNA"]]$counts[, sampled_cells, drop = FALSE])))
    if (nrow(features_df) > 0) {
      plot_list <- pmap(features_df, plotCellComic)
      wrap_plots(plot_list, nrow=1)
    }
  }
  
  output$act4_tcell_cluster_A_examples <- renderPlot({ plot_t_cell_cluster(t_cell_clusters_r()[1]) })
  output$act4_tcell_cluster_B_examples <- renderPlot({ plot_t_cell_cluster(t_cell_clusters_r()[2]) })
  
  observeEvent(input$act4_guess_mouth, { req(input$act4_guess_mouth > 0); rv$act4_q1_correct <- TRUE; rv$act4_q1_feedback <- tags$div(style="margin-top:15px; padding: 10px; background-color: #e8f5e9; border-radius: 5px;", h5(style="color: green; font-weight:bold;", "Exactly!"), p("The 'mouth expression' is the key difference. Our unbiased clustering has revealed two distinct subtypes of T-Cells based on this biological feature!")) }, ignoreInit = TRUE)
  set_act4_wrong_feedback <- function() { rv$act4_q1_correct <- FALSE; rv$act4_q1_feedback <- tags$div(style="margin-top:15px; padding: 10px; background-color: #ffebee; border-radius: 5px;", h5(style="color: red; font-weight:bold;", "Not quite."), p("Take another look at the example cells for the two T-Cell clusters. While other features might vary, only one is consistently different between the two groups. Try again!")) }
  observeEvent(input$act4_guess_moustache, { req(input$act4_guess_moustache > 0); set_act4_wrong_feedback() }, ignoreInit = TRUE)
  observeEvent(input$act4_guess_hair, { req(input$act4_guess_hair > 0); set_act4_wrong_feedback() }, ignoreInit = TRUE)
  observeEvent(input$act4_guess_color, { req(input$act4_guess_color > 0); set_act4_wrong_feedback() }, ignoreInit = TRUE)
  
  output$act4_q1_feedback <- renderUI({ rv$act4_q1_feedback })
  output$act4_q1_is_correct <- reactive({ rv$act4_q1_correct }); outputOptions(output, "act4_q1_is_correct", suspendWhenHidden = FALSE)
  
  observeEvent(input$tcell_difference_bio, { rv$act4_feedback <- tags$div(style="padding: 10px; background-color: #e8f5e9; border-radius: 5px;", h4(style="color: green;", "Correct!"), p("This is a biological difference. The 'mouth expression' is a consistent feature that separates the T-Cells, unlike the 'rollerblades' which were specific to one batch. This reveals two distinct subtypes of T-Cells!")) })
  observeEvent(input$tcell_difference_batch, { rv$act4_feedback <- tags$div(style="padding: 10px; background-color: #ffebee; border-radius: 5px;", h4(style="color: red;", "Not quite!"), p("While batch effects are a major concern, this difference is biological. The 'mouth expression' feature is present in T-cells from both Batch 1 and Batch 2 within their respective clusters. It's not a technical artifact, but a real biological distinction leading to two subtypes.")) })
  output$act4_tcell_feedback <- renderUI({ rv$act4_feedback })
  output$act4_umap_subtypes <- renderPlot({ 
    req(rv$clustered_seurat_soft)
    DimPlot(rv$clustered_seurat_soft, group.by = "sub_cell_type", reduction = "umap_soft") + labs(title = "The Revealed T-Cell Subtypes") 
  })
  output$act4_subtype_confusion <- renderPlot({ req(rv$clustered_seurat_soft); truth_data <- rv$clustered_seurat_soft@meta.data; confusion_df <- as.data.frame(table(Cluster = truth_data$seurat_clusters, Subtype = truth_data$sub_cell_type)); ggplot(data = confusion_df, aes(x = Subtype, y = Cluster, fill = Freq)) + geom_tile() + geom_text(aes(label = Freq), vjust = 0.5, size = 5) + scale_fill_gradient(low = "white", high = "#337ab7") + labs(title = "Seurat Clusters vs. True Subtypes", x = "True Subtype", y = "Seurat Cluster") + theme_minimal(base_size=14) + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5, size = 16)) })
  
  observeEvent(input$act4_q2_no, { rv$act4_q2_feedback <- tags$div(style="margin-top:15px; padding: 10px; background-color: #e8f5e9; border-radius: 5px;", h5(style="color: green; font-weight:bold;", "Correct!")) })
  observeEvent(input$act4_q2_yes, { rv$act4_q2_feedback <- tags$div(style="margin-top:15px; padding: 10px; background-color: #ffebee; border-radius: 5px;", h5(style="color: red; font-weight:bold;", "Incorrect."), p("A strong integration would have forced the two T-cell subtypes together, hiding their differences. Click 'No' to see the proof!")) })
  output$act4_q2_feedback <- renderUI({ rv$act4_q2_feedback })
  
  output$strong_integration_subtypes_plot <- renderPlot({
    req(rv$integrated_seurat_strong)
    DimPlot(rv$integrated_seurat_strong, group.by="sub_cell_type", reduction="umap_strong") + labs(title="Subtypes on Strongly Integrated UMAP")
  })
  
  output$strong_integration_clusters_plot <- renderPlot({
    req(rv$clustered_seurat_strong)
    DimPlot(rv$clustered_seurat_strong, reduction="umap_strong", label=TRUE) + NoLegend() + labs(title="Clustering on Strongly Integrated Data")
  })
}

# ==============================================================================
#  RUN THE APP
# ==============================================================================
shinyApp(ui = ui, server = server)

