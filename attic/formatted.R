
formatted.scorecard <- formattable(scorecard.df,
                                   list(
                                     ModelID = formatter("span", 
                                                         style = style(background = "lightgray",
                                                                       color = "black")),
                                     Status = formatter("span", style = x ~ ifelse(
                                       x == "activated",
                                       style(
                                         background = "green",
                                         color = "white",
                                         font.weight = "bold"
                                       ),
                                       ifelse(
                                         x == "candidate",
                                         style(
                                           background = "yellow",
                                           color = "black",
                                           font.weight = "bold"
                                         ),
                                         ifelse(
                                           x == "deactivated",
                                           style(
                                             background = "red",
                                             color = "white",
                                             font.weight = "bold"
                                           ),
                                           style(
                                             background = "blue",
                                             color = "white",
                                             font.weight = "bold"
                                           )
                                         )
                                       )
                                     ))
                                   ))
formatted.scorecard

