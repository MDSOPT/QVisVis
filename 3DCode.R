
#Random points on a sphere with np=number of points, rad=radius of the sphere

sphere <- function(np, rad){
  i<-1;
  B=matrix(nrow=np,ncol=3)
  while (i<np+1) {i<-i+1; z<-runif(1, -rad, rad);
  theta<-runif(1,0,2*pi); x<-sqrt(rad^2-z^2)*cos(theta); y<-sqrt(rad^2-z^2)*sin(theta);
  B[i-1,1]<-x; B[i-1,2]<-y; B[i-1,3]<-z}
  return(B)
}

#Random points on a torus with np=number of points, tuberad=tube radius, ringrad=ring radius

torus <- function(np, tuberad, ringrad){
  i<-1;
  B=matrix(nrow=np,ncol=3)
  while (i<np+1) {i<-i+1; u<-runif(1, 0, 2*pi); v<-runif(1, 0, 2*pi);
  x<-(ringrad+tuberad*cos(v))*cos(u); y<-(ringrad+tuberad*cos(v))*sin(u); z<-tuberad*sin(v);
  B[i-1,1]<-x; B[i-1,2]<-y; B[i-1,3]<-z}
  return(B)
}

#Random points on a SwissRoll with np=number of points, roll=number of folds or layers in the roll, start=0 should be used if
#the first point is 0,0 otherwise larger values can be used note if start>2*roll the number of rolls is reduced, length= the width of the roll

swissroll <- function(np, roll, start, length){
  i<-1;
  B=matrix(nrow=np,ncol=3)
  while (i<np+1) {i<-i+1; u<-runif(1, start, 2*roll*pi); v<-runif(1, 0, length);
  x<-u*cos(u); y<-u*sin(u); z<-v;
  B[i-1,1]<-x; B[i-1,2]<-y; B[i-1,3]<-z}
  return(B)
}

#Regular sphere user inputs the degrees in radians between adjacent points, pi/6 corresponds to 62 points, so np is not necessary 
#it is calculated by a formula

rsphere <- function(deg, rad){
  merid<-2*pi/deg;
  paral<-merid/2+1;
  np<-merid*paral+2
  B=matrix(nrow=np,ncol=3);
  B[1,1]<-0;B[1,2]<-0;B[1,3]<-rad; 
  B[np,1]<-0;B[np,2]<-0;B[np,3]<--rad;
  for (i in 1:paral) {
    for (j in 1:merid) {z<-rad*cos(deg*i);
    theta<-2*pi*(j-1)/merid; x<-sqrt(rad^2-z^2)*cos(theta); y<-sqrt(rad^2-z^2)*sin(theta);
    B[(i-1)*merid+j+1,1]<-x; B[(i-1)*merid+j+1,2]<-y; B[(i-1)*merid+j+1,3]<-z}}
  return(B)
}

#Regular points on a torus with tuberad=tube radius, ringrad=ring radius
#tubedeg and ringdeg determine number of points pi/6 for both makes 144 points 

rtorus <- function(tubedeg, ringdeg, tuberad, ringrad){
  tubepoints<-2*pi/tubedeg;
  ringpoints<-2*pi/ringdeg;
  B=matrix(nrow=tubepoints*ringpoints,ncol=3)
  for (i in 1:ringpoints) {
    for (j in 1:tubepoints) {
      u<-2*pi*(j-1)/tubepoints; v<-2*pi*(i-1)/ringpoints;
      x<-(ringrad+tuberad*cos(v))*cos(u); y<-(ringrad+tuberad*cos(v))*sin(u); z<-tuberad*sin(v);
      B[(i-1)*tubepoints+j,1]<-x; B[(i-1)*tubepoints+j,2]<-y; B[(i-1)*tubepoints+j,3]<-z}}
  return(B)
}