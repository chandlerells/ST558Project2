rmarkdown::render(input = "ST558-Project-2.Rmd",
                  output_format = "github_document",
                  output_file = "README.md",
                  output_options = list(
                    html_preview = FALSE,
                    toc = TRUE, 
                    toc_depth = 3,
                    toc_float = TRUE))
