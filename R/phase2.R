#-----------------------------------------------------------------------------#
#                                                                             #
#  MATRIX-BASED FLEXIBLE PROJECT PLANNING                                     #
#                                                                             #
#  Written by: Zsolt T. Kosztyan, Aamir Saghir                                #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kzst@gtk.uni-pannon.hu                                         #
#                                                                             #
# Last modified: May 2022                                                     #
#-----------------------------------------------------------------------------#
#' @export
phase2<- function(x,p=0.1,s=5.0){
  if (!requireNamespace("pracma", quietly = TRUE)) {
    stop(
      "Package \"pracma\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if ("PDM_list" %in% class(x)){
    PDM<-x$PDM
  }else{
    if (("PDM_matrix" %in% class(x))||("matrix" %in% class(x))||("array" %in% class(x))||("data.frame" %in% class(x))){
      PDM<-x
    }else{
      stop(
        "truncpdm works only on matix, PDM_matrix, and PDM_list.",
        call. = FALSE
      )
    }
  }
  class(PDM)<-"PDM_matrix"
  n=pracma::size(PDM,1)
  m=pracma::size(PDM,2)
  PDMout=PDM
  if (m>n){
   PDMout[,(n+1):m]=PDMout[,(n+1):m]+(pracma::rand(n,m-n)<p)*(PDM[,(n+1):m]*s)
      for (i in 1:n){                   #demands will be similar to the other demands
      for (j in ((n+1):m)){
        if ((PDM[i,j]>0) && (PDM[i,j]<=1) && (PDMout[i,j]>1))
          PDMout[i,j]=1                  # %Quality should not be greater than 1
      }
    }

  }
  class(PDMout)<-"PDM_matrix"
  if ("PDM_list" %in% class(x)){
    x$PDM<-PDMout
    output<-x
    class(output)<-"PDM_list"
    return(output)
  }else{
    return(PDMout)
  }
}





