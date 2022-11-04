library(tidyverse)

enut=read.csv("products/csv/enut.csv")
enut = enut %>%
  mutate(X=NULL)


par_var_y = "ENT"
par_var_x = c("SEXO","EDAD","ESC")
ncols_vars_x = length(par_var_x)
l=rep(list(0:1),ncols_vars_x)
grid_vars_x=expand.grid(l)[-1,]

names(grid_vars_x)
grid_vars_x = grid_vars_x %>%
  rename(SEXO=Var1,EDAD=Var2,ESC=Var3)

for (row in 1:nrow(grid_vars_x)) {
  output = ""
  for (column in 1:ncol(grid_vars_x)) {
    if(grid_vars_x[row,column]==1){
      output=paste(output,names(grid_vars_x)[column],sep = "+")}}
  output=str_remove(output,"^\\+")
  print(paste("Las variables del modelo son",output))
  output=paste("SAL_SEM ~",output)
  modelo = lm(eval(parse(text = output)),data = enut)
  print(paste("La R cuadrada es: ",summary(modelo)$r.squared,sep = ""))
  print(paste("La R cuadrada ajustada es:",summary(modelo)$adj.r.squared,sep = ""))
}
