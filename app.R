

#

# Gist: https://gist.github.com/anonymous/9da10c816963c91020fc18a7d557268b


# A Shiny Program for illustrating the strategic logic of 
# a subordinate actor choosing to reveal (R), conceal (C), 
# or abandon (A) preferences that are different than those 
# of a superordinate actor.
#
# - Kurt Taylor Gaubatz - 8-14-2016

library(shiny)                         # Make sure shiny is loaded

# A function for drawing ovals
oval = function(xcen, ycen,            # Oval function - center coords --------+
  xlen, ylen,                          # Length of x and y radii               |
  ewidth = 1, ecolor = "black"){       # Line width and color                  |
  t = seq(0, 2 * pi, length = 2000)    # Set up theta for degrees around oval  |
  x = xcen + xlen * cos(t)             # x values for ellipse                  |
  y = ycen + ylen * sin(t)             # y values for ellipse                  |
                                       #                                       |
  lines(x, y,                          # Plot x and y values                   |
    usr = c(0, 1, 0, 1),               # Use 0,1 coordinates                   |
    lty = 2,                           # Use dashed line                       |
    lwd = ewidth,                      # Line width from function call         |
    col = ecolor)                      # Color set by function call            |
}                                      # End of oval function -----------------+



# Create the interactive plot to be called later

TrumpServer = function(input, output) {

  # Reactive expression to build data frame holding input values   
    sliderValues = reactive ({
    
    # Create data frame    
    data.frame(
      Name = c("PA", "PD",  "CandC"),
      Value = as.character(c(input$PA, input$PD, input$CandC)),
      stringsAsFactors=FALSE)
  })
  
    output$DPlot = renderPlot({
      
      plot.new()
      
      text(x = .1, y=.9, label = "SQ Actor")
      text(x = .6, y=.9, label = "Revisionist Actor")
      text(x=.1, y=.6, label = "Cooperative")
      text(x = .4, y = .6, label = "Cooperative")
      text(x = .75, y = .6, label = "Non-Cooperative")
      text(x = .95, y = .6, label = "Discourse\nStage")
      text(x = .1, y=.2, label = "Cooperative")
      text(x = .3, y=.2, label = "Cooperative")
      text(x=.5, y=.2, label="Non-Cooperative")
      text(x=.75, y = .2, label="Non-Cooperative")
      text(x=.95, y=.2, label="Action\nStage")
      text(x=.1, y = .1, label="Confirm")
      text(x=.3, y = .1, label="Abandon")
      text(x = .5, y=.1, label="Conceal")
      text(x = .75, y=.1, label="Reveal")
      
      lines(x=c(.1,.1),y=c(.87,.63))
      lines(x=c(.1,.1),y=c(.57,.23)) 
      lines(x=c(.6,.4),y=c(.87,.63))
      lines(x=c(.6,.75),y=c(.87,.63))
      lines(x=c(.4,.3),y=c(.57,.23))
      lines(x=c(.4,.5),y=c(.57,.23))
      lines(x=c(.75,.75),y=c(.57,.23))
      
      oval(.25,.6,.25,.07, ewidth=1, ecolor="blue")   
     
      # Highlight expected paths
      # Abandon if above AR line but left of critical point
      if(input$CandC <= (1+input$PD/input$PA)/(1+2*input$PD/input$PA)
          & input$PA >= 1/(1+input$PD/input$PA)){
          lines(x=c(.6,.4), y=c(.87, .63), col="#FF000050", lwd=6)
          lines(x=c(.4,.3), y=c(.57, .23), col="#FF000050", lwd=6)
          lines(x=c(.25,.35), y=c(.1,.1), col="#FF000050", lwd=10)
      }
      # Abandon if right of critical point and above AC border
      if(input$CandC >= (1+input$PD/input$PA)/(1+2*input$PD/input$PA)
         & input$PA >= input$CandC/(1+(input$PD/input$PA) - input$CandC * input$PD/input$PA)){
        lines(x=c(.6,.4), y=c(.87, .63), col="#FF000050", lwd=6)
        lines(x=c(.4,.3), y=c(.57, .23), col="#FF000050", lwd=6)
        lines(x=c(.25,.35), y=c(.1,.1), col="#FF000050", lwd=10)
      }      

      # Reveal if below AR line but left of critical point
      if(input$CandC < (1+input$PD/input$PA)/(1+2*input$PD/input$PA)
         & input$PA < 1/(1+input$PD/input$PA)){
        lines(x=c(.6,.75), y=c(.87, .63), col="#FF000050", lwd=6)
        lines(x=c(.75,.75), y=c(.57, .23), col="#FF000050", lwd=6)
        lines(x=c(.7,.8), y=c(.1,.1), col="#FF000050", lwd=10)
      }
      
      # Reveal if right of critical point and below RC border
      if(input$CandC >= (1+input$PD/input$PA)/(1+2*input$PD/input$PA)
         & input$CandC <= 1/(1 + input$PD)){
        lines(x=c(.6,.75), y=c(.87, .63), col="#FF000050", lwd=6)
        lines(x=c(.75,.75), y=c(.57, .23), col="#FF000050", lwd=6)
        lines(x=c(.7,.8), y=c(.1,.1), col="#FF000050", lwd=10)
      }          
      
      # Conceal if right of critical point, above RC border and below AC border
      if(input$CandC > (1+input$PD/input$PA)/(1+2*input$PD/input$PA)
         & input$CandC > 1/(1 + input$PD)
         & input$PA < input$CandC/(1+(input$PD/input$PA) - input$CandC * input$PD/input$PA)){
        lines(x=c(.6,.4), y=c(.87, .63), col="#FF000050", lwd=6)
        lines(x=c(.4,.5), y=c(.57, .23), col="#FF000050", lwd=6)
        lines(x=c(.45,.55), y=c(.1,.1), col="#FF000050", lwd=10)
      }       
            
      
      
    #  if c > criticl point and PA > critical line then abandon path
      
#      if PA < 1/(1+input$PD/input$PA) and C < critical point then reveal 
#      if C > critical val and PA < critical line then reveal
      
#      if C > critical val and PA < crit line and PA > crit line then conceal

    })
 
 
       output$linePlot = renderPlot({
 
    plot(input$CandC, input$PA,                # Set up plot without points
         ylab = "Punishment for Non-cooperative Actions", 
         xlab = "Ability to conceal/coordinate",
         xlim = c(0,1), ylim = c(0,2),
         xaxs="i", yaxs="i",
         type = "n") 

      abline(v=input$CandC, col = "red")
      abline(h=input$PA, col = "blue")
      
      #   Calculate Critical Values

      ARX1 = 0                                 # left end of line between Abandon & Reveal
      ARX2 = (1+input$PD/input$PA)/(1+2*input$PD/input$PA)
      ARY1 = 1/(1+input$PD/input$PA)
      ARY2 = 1 / (1 + input$PD/input$PA)
      
      ACX1 = (1 + input$PD/input$PA) / (1 + 2 * input$PD/input$PA)
      ACX2 = 1
      ACY1 = 1 / (1 + input$PD/input$PA)
      ACY2 = 1
      
      RCX1 = (1 + input$PD/input$PA) / (1 + 2 * input$PD/input$PA)
      RCX2 = 1
      RCY1 = 1 / (1 + input$PD/input$PA)
      RCY2 = 0 
      
          
    lines(x = c(ARX1, ARX2), y = c(ARY1, ARY2))
    lines(x = c(ACX1, ACX2), y = c(ACY1, ACY2))
    lines(x = c(RCX1, RCX2), y = c(RCY1, RCY2))
    
    text(x = .3, y = 1, label = "Abandon")
    text(x = .3, y = .25, label = "Reveal")
    text(x = .95, y=.5, label = "Conceal")

  })

}

# Setup Shiny user interface

TrumpUI = pageWithSidebar(
    # Choosing to Misrepresent
  headerPanel("The Choice to Misrepresent"),
 
  # Sidebar with a slider input for number of bins
  sidebarPanel(
 
   # Explanation
  p("In this decision model a subordinate
    actor decides whether to reveal or 
    conceal their true preferences in
    the face of a disapproving superordinate
    actor." ),
  p("There are two stages:
    First the actor decides whether to reveal their
    preference for non-cooperation. Second 
    the actor decides whether to act
    on their non-cooperative preference."),    
  p("We assume that the payoff for a cooperative
    act is 0, and the payoff for a non-cooperative
    act is 1."),
  p("The three main variables are the ability to 
    conceal and coordinate without revealing
    preferences. the punishment for non-cooperative
    discourse, and the punishment for non-cooperative
    actions"),
   br(),
  
        sliderInput("PA",
                "What is the expected punishment for a non-cooperative act?",
                min = 0,
                max = 2,
                step = .1,
                value = 1),
    sliderInput("PD",
                "What is the expected punishment for non-cooperative discourse?",
                min = 0,
                max = 2,
                step = .1,
                value = .5),
    sliderInput("CandC",
                "How effectively can the actor conceal and coordinate around 
                non-cooperative preferences without being observed by the superordinate actor?",
                min = 0,
                max = 1,
                value = .5)
  ),
  
 
  # Shiny Main Panel
  
  mainPanel(
    
    # panel heading 
    p(h3("The Incentive to Misrepresent")),
    br(), 
    
    plotOutput("DPlot"),  
   
    p("This plot shows the regions in which an actor with non-cooperative preferences will
      choose to conceal, reveal, or abandon those preferences, for any combination of expectations
      about the ability to conceal and the punishment for non-cooperative action. The relative size
      of the regions is affected by the relative level of punishments for discourse v. action."),
     
    plotOutput("linePlot")
      
      
  )
    
)


shinyApp(ui = TrumpUI, server = TrumpServer)

