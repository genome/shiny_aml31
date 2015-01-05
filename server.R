library(shiny)
library(venneuler)
#library(variantAnnotation)
options(shiny.maxRequestSize=50*1024^2) #set a 50MB maximum size for the upload file
#options(shiny.trace=TRUE)
#source("helpers.R")

##------------------------------------------
## these will be moved to a separate file later
cleanInputData <- function(df){
    df$key = paste(df[,1],df[,2],df[,4],df[,5],sep="_")
    return(df)
}

#take two dataframes, each with a "key" column, and create a proportional venn diagram of their overlap
createVenn <- function(a, b, range){

  #use range later to restrict to certain vafs
  allkeys = unique(c(as.character(a$key), as.character(b$key)))
  keyFound = cbind(allkeys %in% a$key, allkeys %in% b$key)
  A = length(which(keyFound[,1]==1 & keyFound[,2]==0))
  B = length(which(keyFound[,1]==0 & keyFound[,2]==1))
  AB = length(which(keyFound[,1]==1 & keyFound[,2]==1))

  v <- venneuler(c(A=A, B=B, "A&B"=AB))
  plot(v)

  col.fn <- function(col, alpha=0.3) {
    col <- hcl(col * 360, 130, 60)
    col <- col2rgb(col)/255
    col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
    col
  }
  vcol <- c(col.fn(v$colors),rgb(130/255,209/255,201/255))
  vlabs <- v$labels

  legend(0.05, .9, legend=c(paste("Validated Only:",A), paste("Uploaded Only:",B), paste("Shared:",AB)), fill=vcol, x="topleft")
  #print sens/spec in bottom left
  usr <- par( "usr" )
  text( usr[ 1 ], usr[ 3 ]+0.1, paste("Sensitivity:",round((AB/A),4)), adj = c( 1, 0 ), pos=4)
  text( usr[ 1 ], usr[ 3 ]+0.05, paste("Positive Predictive Value:",round((AB/(AB+B)),4)), adj = c( 1, 0 ), pos=4)

}

#create histogram showing performance per VAF
createHist <- function(a, b, range){
  v = NULL;
  for(vaf in seq(0,95,5)){
    av = a[a$tum_vaf > vaf & a$tum_vaf <= vaf+5,]
    allkeys = unique(c(as.character(av$key), as.character(b$key)))
    keyFound = cbind(allkeys %in% av$key, allkeys %in% b$key)
    A = length(which(keyFound[,1]==1 & keyFound[,2]==0))
    AB = length(which(keyFound[,1]==1 & keyFound[,2]==1))

    v = rbind(v, data.frame(vaf=vaf, val=A, shared=AB))
  }
  cat(v$val,file=stderr())
  vals = rep(v$vaf,v$val)
  hist(vals,breaks=seq(0,95,5),col=rgb(0,1,0,0.3),xlab="Tumor VAF",main="Valid uploaded variants by VAF")
  vals = rep(v$vaf,v$shared)
  hist(vals,breaks=seq(0,95,5),col=rgb(0,0,1,0.3),add=T,xlab="",main="")
  legend("topright",fill=c(rgb(0,1,0,0.3),rgb(0,0,1,0.3)), legend=c("Validated","Uploaded"))
}


#strip the 'key' column from a data frame for display purposes
stripKeyCol <- function(df){
  return(df[,!(names(df) %in% c("key"))])
}


labelFound<-function(upload,truth){
  upload = cleanInputData(upload)
  upload$in_validation = as.character(upload$key) %in% as.character(truth$key)
  return(stripKeyCol(upload))
}

showUploadRequestMessage <- function(){
    plot(0,0,col="white",xlim=c(1,100),ylim=c(1,100),bty="n",xaxt="n",yaxt="n",ylab="",xlab="")
    text(25,75,"please upload a file of predictions")
}

##---------------------------------------------------------


# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  compTable <- reactive({
    compList = input$list
    file = "list.platinum"
    if(compList == "gold"){
      file = "list.gold"
    }
    read.table(file,header=T,sep="\t")
  })

  #inputData <- renderTable({
  inputData <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$tabfile

    if (is.null(inFile))
      return(NULL)

    read.table(inFile$datapath,header=F,sep="\t", col.names=c("Chr","St","Sp","Ref","Var"))
  }) 


  ##------------------------------------
  ##generate the venn diagram (tab 1)
  output$plot <- renderPlot({

  if(is.null(inputData())){
    showUploadRequestMessage()
  } else {
    createVenn(compTable(), cleanInputData(inputData()), input$range)
  }
 })
  
  ##---------------------------------------------------------
  ## generate a histogram view of performance
  output$hist <- renderPlot({
    if(is.null(inputData())){
      showUploadRequestMessage()
    } else {
      createHist(compTable(), cleanInputData(inputData()), input$range)
    }
  })

  ##---------------------------------------------------------
  ## Generate an HTML table view of the uploaded data (tab 3)
  output$uploaded <- renderDataTable({
     if(is.null(inputData())){
       data.frame(message="please upload a file of predictions")
     } else {
       labelFound(data.frame(inputData()),data.frame(compTable()))
     }},
     options = list(pagelength=25))
 
  ##---------------------------------------------------------
  ## Generate an HTML table view of the truth-set (tab 4)
  output$truth <- renderDataTable({
    stripKeyCol(data.frame(compTable()))},
    options = list(pagelength=25))

})
