## theme


#library(extrafont)
#font_import() 
#extrafont::font_import(paths = "/System/Library/Fonts/", prompt = F, pattern = "Helvetica")
                                #C:/Users/*insert your user name*/AppData/Local/Microsoft/Windows/Fonts", pattern = ".TTF")

#CTA Update: Using LIST (with named elements) for 'rad_colors$primary' style indexing...
# Uses HIDDEN elements as to not crowd the R data environment
# adds FUNCTION to show colors and names (the hidden values)

#Define branded color sets
.base_colors <- list("primary_blues" = c("#005287","#00354e",  "#0278af", "#5a5c5d", "#c1c2c4"),
                     
                    "primary_reds" = c("#c42032", #visitech red (or close....)
                    "#850101", #: A deep, dark red.
                    "#6D2B2C",#: A more muted, desaturated red.
                    "#614041",# A reddish-brown or dusky rose color.
                    "#555556", # A medium, neutral grey.
                    "#292323"), # A very dark grey, close to black
                    
                    "combined_colors" = c(
                      "#005287", "#c42032",
                      "#00354e", "#850101", 
                      "#0278af", "#6D2B2C", 
                      "#c1c2c4", "#5a5c5d", 
                      "#685786", "#ffcc05",
                      "#0278af", "#00354e")
                    )

## USE BRANDED COLOR SETS WITH::
#scale_color_manual(values = .base_colors$primary_blues)

# Custom ggplot theme based on RAD branding with color palettes - ADJUSTS TEXT NATIVELY
theme_cta_resize <- function() {
  theme_minimal(base_family = "Arial") + 
    theme(
      # Title styling
      plot.title = element_text(
        family = "Arial", face = "bold", size = rel(1.25), color = "#005287", hjust = 0.5, margin = margin(b = 10)
      ),
      # Subtitle styling
      plot.subtitle = element_text(
        family = "Arial", size = rel(1), color = "#5a5c5d", hjust = 0.5, margin = margin(b = 10)
      ),
      # Axis title styling
      axis.title = element_text(
        family = "Arial", size = rel(.9), color = "#005287", face = "bold"
      ),
      # Axis text styling
      axis.text = element_text(
        family = "Arial", size = rel(.75), color = "#5a5c5d"
      ),
      # Legend title and text styling
      legend.title = element_text(
        family = "Arial", size = rel(.75), color = "#005287", face = "bold"
      ),
      legend.text = element_text(
        family = "Arial", size = rel(.65), color = "#5a5c5d"
      ),
      # Panel background color
      panel.background = element_rect(fill = "#ffffff", color = NA),
      # Grid lines styling
      panel.grid.major = element_line(color = "#c1c2c4"),  # Light grey grid lines
      panel.grid.minor = element_blank(),  # No minor grid lines
      # Strip background for facet labels
      strip.background = element_rect(fill = "#0278af", color = "#005287"),
      strip.text = element_text(family = "Arial", size = rel(.9), color = "#ffffff", face = "bold")
    )
}

