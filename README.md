
<!-- README.md is generated from README.Rmd. Please edit that file -->
Ecosystem Context for Stock Advice (ECSA)
=========================================

The goal of the ECSA project is to create bespoke documents containing contextual ecosystem information for managed stocks on the Northeast Continental Shelf. The document creation process relies on the use of survey strata (as defined by the NEFSC [Bottom Trawl Survey](https://www.nefsc.noaa.gov/femad/ecosurvey/mainpage/)) to identify stock areas from which time series of area-averaged ecosystem information can be drawn. For each area of interest, we have automated the aggregation of season-specific time series of ocean temperature and salinity (surface and bottom), zooplankton composition and abundance, and chlorophyll concentration. Each document contains time series of estimated area occupied by a given stock within the specified strata and within the ecosystem as a whole. We also provide estimates for minimum population size (abundance and biomass) based on yearly restratification of habitat given estimates of habitat occupancy.

Developing an ECSA project
--------------------------

The document creation process involves three R functions: `create_template.R`, `merge_to_bookdown.R`, and `render_ecsa.R`. The general workflow is as follows:

`{r, setup, include=FALSE} library(nomnoml)`

``` {nomnoml}
#stroke: orange
#.box: fill=#8f8 dashed visual=ellipse
[A]-[B]
[B]-[<box>C]
```

\`\`\`\` <!--html_preserve-->

<script type="application/json" data-for="htmlwidget-359a8735a1cfbb8886cf">{"x":{"code":"\n  #fill: #FEFEFF\n  #lineWidth: 1\n  #zoom: 4\n  #direction: right\n   #stroke: orange\n#.highlight: fill=#8f8 dashed visual=ellipse\n[A]-[B]\n[B]-[<highlight>C]","svg":false},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
``` nomnoml
#stroke: #a86128
[<frame>Decorator pattern|
  [<abstract>Component||+ operation()]
  [Client] depends --> [Component]
  [Decorator|- next: Component]
  [Decorator] decorates -- [ConcreteComponent]
  [Component] <:- [Decorator]
  [Component] <:- [ConcreteComponent]
]
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-fbe626686516ba8c4a9e">{"x":{"code":"\n  #fill: #FEFEFF\n  #lineWidth: 1\n  #zoom: 4\n  #direction: right\n   #stroke: #a86128\n[<frame>Decorator pattern|\n  [<abstract>Component||+ operation()]\n  [Client] depends --> [Component]\n  [Decorator|- next: Component]\n  [Decorator] decorates -- [ConcreteComponent]\n  [Component] <:- [Decorator]\n  [Component] <:- [ConcreteComponent]\n]","svg":false},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
Note: The ECSA is not yet in R package format. Hence, functions must be sourced from the `R/` directory during the document development process. Template `.Rmd` files source the functions by default.
