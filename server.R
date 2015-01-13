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

subsetByVaf <- function(df,range){
  if("tum_vaf" %in% names(df)){
    return(df[df$tum_vaf >= range[1] & df$tum_vaf <= range[2],])
  }
  return(df)
}

##----------------------------------------------
##take two dataframes, each with a "key" column, and create a proportional venn diagram of their overlap
createVenn <- function(a, b, range){

  #use range later to restrict to certain vafs
  a = subsetByVaf(a,range)
  b = subsetByVaf(b,range)

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
  text( usr[ 1 ], usr[ 3 ]+0.15, paste("VAF range: ",range[1],"-",range[2],sep=""),adj = c( 1, 0 ), pos=4)


}

#create histogram showing performance per VAF
createHist <- function(a, b){
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

#label uploaded variants as found in truthset or not
labelFound<-function(upload,truth){
  upload = cleanInputData(upload)
  upload$in_validation = as.character(upload$key) %in% as.character(truth$key)
  return(stripKeyCol(upload))
}

#placeholder plot that requests an upload
showUploadRequestMessage <- function(){
    plot(0,0,col="white",xlim=c(1,100),ylim=c(1,100),bty="n",xaxt="n",yaxt="n",ylab="",xlab="")
    text(25,75,"please upload a file of predictions")
}

#get the name of the list file from the 
getListFile <- function(truthList){
    file="list.platinum"
    if(truthList == "gold"){
        file = "list.gold"
    }    
    return(file)
}

##---------------------------------------------------------


## Define server logic for random distribution application
shinyServer(function(input, output) {
    ## Reactive expression to read the radio buttons and use the correct
    ## list (platinum or gold) for comparison
    compTable <- reactive({
        truthList = input$list
        read.table(getListFile(truthList),header=T,sep="\t")
    })
    
    ## reactive expression to read the file input by the user
    inputData <- reactive({
        ##inFile will be NULL initially
        inFile <- input$tabfile
        if (is.null(inFile))
            return(NULL)
        
        read.table(inFile$datapath,header=F,sep="\t", col.names=c("Chr","St","Sp","Ref","Var"))
    }) 
    
    ##reactive expression telling us if data has been uploaded or not
    output$fileUploaded <- reactive({
        return(!is.null(inputData()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
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
            createHist(compTable(), cleanInputData(inputData()))
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
        options = list(pagelength=25)
    )
    
    ##---------------------------------------------------------
    ## Generate an HTML table view of the truth-set (tab 4)
    output$truth <- renderDataTable({
        stripKeyCol(data.frame(compTable()))},
        options = list(pagelength=25)
    )
    
    ##download links
    output$downloadUserList <- downloadHandler(
        filename = "annotated_variants.tsv",
        content = function(file) {
            if(length(data.frame(inputData())) > 0){ #if file uploaded already
                write.table(stripKeyCol(labelFound(data.frame(inputData()),data.frame(compTable()))),file,sep="\t",quote=F,row.names=F)
            } else { #return an empty table, because hey, that's what they asked for...
                write.table(c(),"/tmp/zz",quote=F)
            }
        }
    )
    
    output$downloadTruthList <- downloadHandler(
        filename = "truth_list.tsv",
        content = function(file) {          
            write.table(stripKeyCol(data.frame(compTable())),file,sep="\t",quote=F,row.names=F)
        }
    )
    
})
