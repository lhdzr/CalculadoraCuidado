library(tidyverse)

enut=read.csv("products/csv/enut.csv")
enut = enut %>%
  mutate(X=NULL)

par_var_x = c("SEXO","EDAD","ESC","TIEMPO_TOTAL","PAREN")
ncols_vars_x = length(par_var_x)
l=rep(list(0:1),ncols_vars_x)
grid_vars_x=expand.grid(l)[-1,]

names(grid_vars_x)
grid_vars_x = grid_vars_x %>%
  rename(SEXO=Var1,EDAD=Var2,ESC=Var3,TIEMPO_TOTAL=Var4,PAREN=Var5)
rownames(grid_vars_x) <- 1:nrow(grid_vars_x)

### Generar dataframe para describir los modelos generados
modelos=data.frame(variables=character(0),R2=numeric(0),R2_ajustada=numeric(0))

### Hacer loop por cada fila y columna, e incluir en el modelo solamente aquellas variables con 1 en esa fila
for (row in 1:nrow(grid_vars_x)) {
  output = ""
  for (column in 1:ncol(grid_vars_x)) {
    if(grid_vars_x[row,column]==1){
      output=paste(output,names(grid_vars_x)[column],sep = "+")}}
  output=str_remove(output,"^\\+")
  ### Incluir las variables usadas en cada modelo, así como sus R2 normal y ajustada.
  modelos[row,"variables"] = output
  output=paste("SAL_SEM ~",output)
  modelo = lm(eval(parse(text = output)),data = enut)
  modelos[row,"R2"] = summary(modelo)$r.squared
  modelos[row,"R2_ajustada"] = summary(modelo)$r.squared
}

modelos %>%
  arrange(desc(R2_ajustada))
