
generate <- function() {
  
  make_rtt <- function(i) {
    data.frame(
      tag = sprintf("gw%02d", i),
      rtt = rnorm(1000, mean = runif(1, 3.5, 5.5), sd = runif(1, 0.5, 2.5)),
      room = ifelse(i < 9, "a", "b")
    )
  }
  
  set.seed(0)
  ldply(seq(1, 16), make_rtt)
}

make_colormap <- function(filename) {
  within(
    read.csv(filename, header = F, col.names = c("r", "g", "b")), {
      rgb <- sprintf("#%02X%02X%02X", floor(r * 255), floor(g * 255), floor(b * 255))
    }
  )
}

c = c(
"#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", "#000000", 
"#800000", "#008000", "#000080", "#808000", "#800080", "#008080", "#808080", 
"#C00000", "#00C000", "#0000C0", "#C0C000", "#C000C0", "#00C0C0", "#C0C0C0", 
"#400000", "#004000", "#000040", "#404000", "#400040", "#004040", "#404040", 
"#200000", "#002000", "#000020", "#202000", "#200020", "#002020", "#202020", 
"#600000", "#006000", "#000060", "#606000", "#600060", "#006060", "#606060", 
"#A00000", "#00A000", "#0000A0", "#A0A000", "#A000A0", "#00A0A0", "#A0A0A0", 
"#E00000", "#00E000", "#0000E0", "#E0E000", "#E000E0", "#00E0E0", "#E0E0E0"
)

c16 = c(
    "#ff0000", # red
    "#ff8000", # orange
    "#996633", # brown
    "#00ffff", # cyan
    "#12860d", # dark green
    "#8a00fd", # light purple
    "#00ff00", # green
    "#4c4c4c", # grey
    
    "#aeff18", # lime
    "#ffff00", # yellow
    "#718371", # light grey
    "#ff00ff", # magenta
    "#0000ff", # blue
    "#f16197", # rose
    "#66ccff", # light blue
    "#fd9f68"  # salmon
    )
    

