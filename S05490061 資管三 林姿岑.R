#期末報告S05490061_林姿岑_資管四

library(readr)
abalone<- read_csv("abalone.data", col_names =  FALSE)
View(abalone)
# Sex / nominal / -- / M, F, and I (infant) 性別/名義/-/ M，F和I（嬰兒）
# Length / continuous / mm / Longest shell measurement長度/連續/毫米/最長外殼尺寸
# Diameter / continuous / mm / perpendicular to length直徑/連續/毫米/垂直於長度
# Height / continuous / mm / with meat in shell高度/連續/毫米/帶外殼
# Whole weight / continuous / grams / whole abalone整重/連續/克/整隻鮑魚
# Shucked weight / continuous / grams / weight of meat去皮重量/連續重量/克/肉重Viscera weight / continuous / grams / gut weight (after bleeding) 內臟重量/連續重量/克/腸重量（出血後）
# Shell weight / continuous / grams / after being dried殼重/連續/克/乾後
# Rings / integer / -- / +1.5 gives the age in years環/整數/-/ +1.5給出年齡
abaloneBackUp <- abalone
names(abalone)
# [1] "X1" "X2" "X3" "X4" "X5" "X6" "X7" "X8" "X9"
names(abalone) <-c("Sex","Length","Diameter","Height","WholeWeight", "ShuckedWeight", "VisceraWeight", "ShellWeight", "Rings")
names(abalone)
# [1] "Sex"           "Length"        "Diameter"      "Height"        "WholeWeight"   "ShuckedWeight"
# [7] "VisceraWeight" "ShellWeight"   "Rings" 

install.packages("dplyr")
library(dplyr)

#使用select()函式選則要分析的欄位
abaloneNumType <- select(abalone,Length:ShellWeight)#選出Length到ShellWeight之間的所有變數
names(abaloneNumType)
# [1] "Length"        "Diameter"      "Height"        "WholeWeight"   "ShuckedWeight" "VisceraWeight"
# [7] "ShellWeight" 


#使用filter()函式選要分析的觀察值
(abaloneFilter01 = filter(abalone, Sex == "I"))
#從性別的變數取I出來
# Sex   Length Diameter Height WholeWeight ShuckedWeight VisceraWeight ShellWeight Rings
# <chr>  <dbl>    <dbl>  <dbl>       <dbl>         <dbl>         <dbl>       <dbl> <dbl>
#   1 I      0.33     0.255  0.08        0.205        0.0895        0.0395       0.055     7
# 2 I      0.425    0.3    0.095       0.352        0.141         0.0775       0.12      8
# 3 I      0.355    0.28   0.085       0.290        0.095         0.0395       0.115     7
# 4 I      0.38     0.275  0.1         0.226        0.08          0.049        0.085    10
# 5 I      0.24     0.175  0.045       0.07         0.0315        0.0235       0.02      5
# 6 I      0.205    0.15   0.055       0.042        0.0255        0.015        0.012     5

(abaloneFilter02 = filter(abalone, WholeWeight >= mean(WholeWeight) ))
#找出總重量大於等於平均的資料(所有鮑魚的總重量)
# Sex   Length Diameter Height WholeWeight ShuckedWeight VisceraWeight ShellWeight Rings
# <chr>  <dbl>    <dbl>  <dbl>       <dbl>         <dbl>         <dbl>       <dbl> <dbl>
# 1 F      0.55     0.44   0.15        0.894         0.314         0.151       0.32     19
# 2 F      0.565    0.44   0.155       0.940         0.428         0.214       0.27     12
# 3 F      0.615    0.48   0.165       1.16          0.513         0.301       0.305    10
# 4 F      0.56     0.44   0.14        0.928         0.382         0.188       0.3      11


#使用mutate()增加新變數欄位
abaloneMutate <- mutate(abalone, LengthVsRings = Length / Rings)
#計算增加新的變數abaloneMutate=總長度除以環數
names(abaloneMutate)
# [1] "Sex"           "Length"        "Diameter"      "Height"        "WholeWeight"  
# [6] "ShuckedWeight" "VisceraWeight" "ShellWeight"   "Rings"         "LengthVsRings"

#使用group_by()函數的功能為設定分組依據，並與summarise()函式合併使用
abaloneMutate %>% group_by(Sex) %>% summarise(count = n(), WholeLengthMean = mean(Length))
#把用summarise()函式產生出來的資料集依據性別丟給group_by排序,接著做出個數及平均值出來
# A tibble: 3 x 3
# Sex   count WholeLengthMean
# <chr> <int>           <dbl>
# 1 F      1307           0.579
# 2 I      1342           0.428
# 3 M      1528           0.561


#4.用ggplot2畫圖
install.packages("ggplot2")
library(ggplot2)

qplot(Diameter,Height,data=abalone,color=Sex)
#直徑跟高度的散佈圖並依據性別來做區分可以發現兩者為正相關r

qplot(Length, data = abalone, fill = Sex)
#長度直方圖依據性別區分

ggplot(abalone, aes(x = abalone$Diameter, y = abalone$WholeWeight)) +geom_boxplot()
#使用盒狀圖表達直徑跟重量的關系

#5 rattle畫5個圖
install.packages("rattle")
install.packages("RGtk2")
library(RGtk2)
library(rattle)
rattle()


