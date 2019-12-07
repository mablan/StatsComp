status<-function(x,y) {
  diferencia = abs(x-y)
  mayor=max(x,y)
  if ((x < 0) | (y<0)) {
    status="imposible" 
    return(status)
  }
  
  if ((mayor > 9) & (diferencia > 3)) {
    status="imposible" 
    return(status)
  }
  
  

  if (mayor <= 8) {
    status="sin terminar"
    return(status)
  }
  if (mayor>=9) {
    if (diferencia<2) {
      status="sin terminar"
      return(status)
    } 
    else if (mayor==x) {
          status="Gana 1"
          return(status)
          } else if (mayor==y) {
                  status="Gana 2"
                  return(status)
                  } else {
                    status="imposible"
                    return(status)
                  }
        }
    
  }