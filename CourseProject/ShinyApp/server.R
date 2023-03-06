library(shiny)

plotmat <- function(mat, clr, ttl, psz=600, cx=1.0) {
  #
  # plotmat(): Simple plotting using matrix mat (filled with 0/nonzero int).
  # Where: mat - matrix; clr - color; ttl - plot title; psz - picture size; 
  #        cx - cex or scale.
  #
  
  m = nrow(mat); 
  d = 0; 
  X=NULL; 
  Y=NULL;
  
  # Building X and Y arrays for plotting from not equal to zero values in mat.
  for (i in 1:m) {
    for (j in 1:m) {
      if(mat[i,j]==0) next 
      else {
        d=d+1; X[d]=i; Y[d]=j
      } 
    }
  };
  
  # Plotting
  if (ttl!="") {
    figPlot <- plot(X,Y, main=ttl, axes=FALSE, xlab="", ylab="", col=clr, pch=20, cex=cx)
  }
  else {
    par(mar=c(0,0,0,0));
    figplot <- plot(X,Y, axes=FALSE, xlab=NULL, ylab=NULL, col=clr, pch=20, cex=cx)
  };
  
  return(figPlot)
}

matkronpow <- function(m, n) {
  ## Kronecker power of a matrix.
  ## Where: m - initial matrix, n - power.
  #
  if (n<2) return (m)
  
  r <- m
  n <- n-1
  
  for(i in 1:n) r <- r%x%m

  return (r)
}

gpKronFractal <- function(m, n, clr, ttl, psz=600, cx=1.0) {
  #  ## Generate and plot Kronecker product based fractals.
  ## gpKronFractal(m, n, pf, clr, ttl, psz, cx):
  ## Where: m - initial matrix (filled with 0/int); n - order of the fractal;
  ## clr - color; ttl - plot title; psz - picture size; cx - cex.
  #
  if(ttl!="") ttl=paste0(ttl,", order ", n);
  
  r = matkronpow(m, n);
  plotmat(r, clr, ttl, psz, cx);
}

pIfsFractal <- function(fn, n, clr, ttl, psz=600, cx=0.5)  {
  #-----------------
  ## Plotting fractals using IFS style
  ## Plotting is based on already calculated M x 7 table of coefficients
  ## in the input file.
  ## Note: 1. Input ifs-file should be dat-file; output is set as png-file.
  ##       2. Swap 2nd and 3rd column if you've got data used in Java,
  ##          JavaScript, etc.
  #
  ## pIfsFractal(fn, n, clr, ttl, psz, cx): Plot fractals using IFS style.
  ## Where: fn - file name; n - number of dots; clr - color; ttl - plot title,
  ##        psz - plot size, cx - cex.
  #-----------------
  
  # df - data/ifs file name;
  df <- paste0("GPRFDATA/", fn,".dat");
  
  # Reading a complete data table from the file: space delimited, no header.
  # Table has any number of rows, but always 7 columns is a must.
  (Tb = as.matrix(read.table(df, header=FALSE)))
  tr = nrow(Tb)
  
  # Creating matrix M1 from 1st 4 columns of each row.
  M1 = vector("list",tr);
  for (i in 1:tr)
    M1[[i]] = matrix(c(Tb[i,1:4]),nrow=2)
  
  # Creating matrix M2 from columns 5,6 of each row.
  M2 = vector("list",tr);
  for (i in 1:tr)
    M2[[i]] = matrix(c(Tb[i,5:6]),nrow=2)
  
  ## Creating matrix M3 (actually a vector) from column 7 of each row.
  M3 = c(Tb[1:tr,7])
  
  x = numeric(n); 
  y = numeric(n);
  x[1] = y[1] = 0;
  
  # Main loop
  for (i in 1:(n-1)) {
    k = sample(1:tr, prob=M3, size=1);
    M = as.matrix(M1[[k]]);
    z = M%*%c(x[i],y[i]) + M2[[k]];
    x[i+1] = z[1];
    y[i+1] = z[2];
  }
  # Plotting
  if (ttl!="") {
    figPlot <- plot(x,y, main=ttl, axes=FALSE, xlab="", ylab="", col=clr, pch=20, cex=cx)
  }
  else {
    par(mar=c(0,0,0,0));
    figPlot <- plot(x,y, axes=FALSE, xlab=NULL, ylab=NULL, col=clr, pch=20, cex=cx)
  };
  
  return(figPlot)
}

# Define server logic required to draw a histogram
function(input, output, session) {

  if(!exists('fractals')) load(file = 'GPRFDATA/fractals.RData')
  if(!exists('matrices')) load(file = 'GPRFDATA/matrices.RData')
  
  reactiveChoices <- reactive({
    if (input$fStyle == "Matrix") return(names(matrices))
    else if (input$fStyle == "Iterated Function") return(names(fractals))
  })
  
  observe({
    updateSelectInput(session, "fPatt", choices = reactiveChoices())
  })
  
  output$distPlot <- renderPlot({
    if(input$fStyle == "Matrix")
      gpKronFractal(matrices[[input$fPatt]], 4, "blue", input$fPatt)
    else if(input$fStyle == "Iterated Function") {
      pIfsFractal(fractals[input$fPatt], 100000, 
                  "dark green", input$fPatt, 800, 0.25)
    }
  }, height = 600, width = 600)
}
