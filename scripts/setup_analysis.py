import os

# Filepaths
dir_py = os.path.dirname(os.path.realpath('__file__'))
dir_proj = os.path.dirname(os.path.realpath(dir_py))
dir_papers = os.path.join(dir_proj,'papers')
dir_products = os.path.join(dir_proj,'products')
dir_data = os.path.join(dir_proj,'data')

tvivienda = os.path.join(dir_data,"conjunto_de_datos_enut_2019_csv\conjunto_de_datos_enut_2019\conjunto_de_datos_tvivienda_enut_2019\conjunto_de_datos\conjunto_de_datos_tvivienda_enut_2019.csv")
thogar = os.path.join(dir_data,"conjunto_de_datos_enut_2019_csv\conjunto_de_datos_enut_2019\conjunto_de_datos_thogar_enut_2019\conjunto_de_datos\conjunto_de_datos_thogar_enut_2019.csv")
tsdem = os.path.join(dir_data,"conjunto_de_datos_enut_2019_csv\conjunto_de_datos_enut_2019\conjunto_de_datos_tsdem_enut_2019\conjunto_de_datos\conjunto_de_datos_tsdem_enut_2019.csv")
tmodulo = os.path.join(dir_data,"conjunto_de_datos_enut_2019_csv\conjunto_de_datos_enut_2019\conjunto_de_datos_tmodulo_enut_2019\conjunto_de_datos\conjunto_de_datos_tmodulo_enut_2019.csv")
var_sel = os.path.join(dir_data,"variables_seleccionadas.csv")
