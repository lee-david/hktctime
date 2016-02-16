library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

	# Application title
	titlePanel("Hong Kong TC Signal Time of Issuance Probability Calculator"),

	# Sidebar with a slider input for the number of bins
	sidebarLayout(
		sidebarPanel(width = 4,
			fluidRow("T+0",
				column(3,textInput("t0lat",label="Lat",value="")),
				column(3,textInput("t0lon",label="Lon",value="")),
				column(3,textInput("t0str",label="Str",value=""))
			),
			fluidRow("T+12",
				column(3,textInput("t12lat",label="Lat",value="")),
				column(3,textInput("t12lon",label="Lon",value="")),
				column(3,textInput("t12str",label="Str",value=""))
			),
			fluidRow("T+24",
				column(3,textInput("t24lat",label="Lat",value="")),
				column(3,textInput("t24lon",label="Lon",value="")),
				column(3,textInput("t24str",label="Str",value=""))
			),
			fluidRow("T+36",
				column(3,textInput("t36lat",label="Lat",value="")),
				column(3,textInput("t36lon",label="Lon",value="")),
				column(3,textInput("t36str",label="Str",value=""))
			),
			fluidRow("T+48",
				column(3,textInput("t48lat",label="Lat",value="")),
				column(3,textInput("t48lon",label="Lon",value="")),
				column(3,textInput("t48str",label="Str",value=""))
			),
			fluidRow("T+72",
				column(3,textInput("t72lat",label="Lat",value="")),
				column(3,textInput("t72lon",label="Lon",value="")),
				column(3,textInput("t72str",label="Str",value=""))
			),
			fluidRow("Issued?",
				column(3,checkboxInput("cursig1",label="T1",value=FALSE)),
				column(3,checkboxInput("cursig3",label="T3",value=FALSE)),
				column(3,checkboxInput("cursig8",label="T8",value=FALSE))
			),
			fluidRow(
				submitButton("Submit")
			)
		),
		# Show a plot of the generated distribution
		mainPanel(
			fluidRow(h4('Probabilities'),
				tableOutput("probtable")
			),
			fluidRow(h4('Not issued throughout (valid only if currently not issued)'),
				tableOutput("noiss")
			),
			fluidRow(h4('Not cancelled throughout (valid only if currently issued)'),
				tableOutput("nocnl")
			)
		)
	)
))
