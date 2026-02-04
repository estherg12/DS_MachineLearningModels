# Matrix Factorization
# Grade scores 100 students in 24 subjects
set.seed(1987)
n <- 100 # estudiantes
k <- 8 # 8 exámenes por cada bloque temático
# Matriz de covarianza 3x3 (valores de 1 en la diagonal)
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) # 0.75, 0.5, etc son correlaciones entre Math, Science y Arts
# 100 filas de datos aleatorios de Distribución Normal Multivariante
m <- MASS::mvrnorm(n, rep(0, 3), Sigma) # cada fila tiene 3 valores (promedio de Math, Science y Arts de cada alumno)
m <- m[order(rowMeans(m), decreasing = TRUE),] # ordena estudiantes de mejor a peor según su promedio en las 3 áreas

# m %x% matrix(rep(1, k), nrow = 1) expande las 3 columnas de promedio a 24 columnas (3 áreas * 8 test).
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3) # rnorm añade ruido aleatorio (variabilidad individual en cada examen)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"), # nombres de las columnas
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

# ---- Q1: visualizar los 24 test para los 100 alumnos ----
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu")) # crea una paleta de rojo a azul (alto a bajo)
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, 
        t(x[rev(rows),,drop=FALSE]), # transpone la matriz y voltea las filas, ya que image lee matrices de abajo a arriba 
        xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5) # dibuja líneas de cuadrícula
  axis(side = 1, cols, colnames(x), las = 2) # nombres de materias
}

my_image(y)
# The students that test well are at the top of the image and there seem to be three groupings by subject.

# ---- Q2: correlación ----
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# There is correlation among all tests, but higher if the tests are in science and math and even higher within each subject.

# ---- Q3: SVD -----
s <- svd(y) # devuelve una lista con u (matriz Nxp), v (matriz pxp), d (vector valores diagonal)
names(s)
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))
ss_y <- colSums(y^2) # suma de cuadrados de columnas y, representa variabilidad total de datos
y_v <- y %*% s$v # transformación YV (multiplicación matriz original x ortogonal V)
ss_yv <- colSums(y_v^2) # suma de cuadrados de columnas YV (transformada)
sum(ss_y)
sum(ss_yv)

# ---- Q4: plot ss_y vs ss_yv ----
par(mfrow = c(1, 2))
plot(ss_y)
plot(ss_yv)
# ss_yv is decreasing and close to 0 for the 4th column and beyond.
# the variability of the columns of YV is decreasing.
# relative to the first three, the variability of the columns beyond the third is almost 0.

# ---- Q5: ss_yv VS D ----
# YV=UD and because U is orthogonal, we know that the sum of squares of the columns of UD are the diagonal entries of D squared
diagonales_d <- s$d # valores singulares (diagonal)
raiz_ss_yv <- sqrt(ss_yv) # raíz cuadrada de suma de cuadrados de YV 
plot(diagonales_d, raiz_ss_yv)
abline(0, 1, col = "red", lty = 2)


# ---- Q6: proporción variabilidad explicada x 3 PC ----
sum(s$d[1:3]^2) / sum(s$d^2)

# ---- Q7: UD = [U1 d1,1 , U2 d2,2 , ... , Up dp,p] ----
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

# ---- Q8: average score for each student ----
par(mfrow = c(1, 1))
plot(rowMeans(y), # promedio x filas 
     s$u[,1]*s$d[1]) # primera columna
cor(rowMeans(y), 
     s$u[,1]*s$d[1])
# There is a linear relationship between the average score for each student and U1 d1,1.

# ---- Q9: primera columna de V vs promedio ----
my_image(s$v)
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
s$v[,1]
# The first column is very close to being a constant, 
# which implies that the first column of YV is the sum of the rows of Y multiplied by some constant, 
# and is thus proportional to an average.

# ---- Q10: Y=U1 d1,1 V1^(T) + U2 d2,2 V2^(T) + ... + Up dp,p Vp^(T) ----
# plot U1 vs V1^(T)
# Perfiles de Estudiantes
plot(s$u[,1], # nivel relativo de cada uno de los 100 estudiantes
     ylim = c(-0.25, 0.25)) # fija el eje para comparar la escala con V
# Pesos de exámenes
plot(s$v[,1], # contribución de cada uno de los 24 exámenes al factor común
     ylim = c(-0.25, 0.25))

# Reconstruir e imprimir la imagen del Primer Componente (PC)
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% # aproximación del rando * transpuesta de v1
                   t(v[, 1, 
                       drop=FALSE]))) # asegura mantener dimensiones de matriz al extraer 1 columna
my_image(y) # imagen de la matriz origianl Y para comparar cuanto ha sido capturado por el primer componente

# ---- Q11: residuos y Ortogonalidad de componentes ----
# Extracción del promedio general (primera capa) para dejar el residuo (patrones secundarios)
resid <- y - with(s, # matriz original - reconstrucción PC1 (factor común)
                  (u[, 1, drop=FALSE]*d[1]) %*% 
                    t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1)) # correlación de residuos
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2) # una vez quitado el promedio, lo que une a los exámenes es su temática

# Gráfico del 2º vector de U -> asilar el Efecto Especialidad: separar a los estudiantes que son mejores en Ciencias/Matemáticas de los que son mejores en Artes
plot(s$u[,2], ylim = c(-0.5, 0.5)) # alumnos con mayor diferencia entre notas de ciencias y artes
plot(s$v[,2], ylim = c(-0.5, 0.5)) # pesos de exámenes en el factor 2, valores positivos para unas materias y negativos en otras
with(s, # reconstrucción del SEGUNDO COMPONENTE
     my_image((u[, 2, drop=FALSE]*d[2]) %*% # el SC de SVD explica la mayor parte posible del residuo
                t(v[, 2, drop=FALSE]))) # segundo valor singular
my_image(resid) # residuo total

# ---- Q12: Aislando el Factor de Especialización Técnica (Math vs. Science) ----
# Eliminar los DOS primeros componentes
resid <- y - with(s, # lo que NO explican ni el promedio ni el contraste Ciencias - Arte
                  sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% # multiplica las 2 primeras columnas de U x sus valores d1 y d2
                    t(v[, 1:2])) # producto matricial con la transpuesta de las 2 primeras columnas de V
# Visualizar la correlación de este segundo residuo 
my_image(cor(resid), # calcula la matriz de correlación del residuo
         zlim = c(-1,1)) # fija la escala de colores para que el rojo sea 1 y azul -1
# las correlaciones de arte han casi desaparecido

axis(side = 2, # eje Y
     1:ncol(y), # posiciones de marcas del eje 24 columnas (y) -> 1:24
     rev(colnames(y)), # define el texto de las etiquetas a escribir (orden invertido xq image se dibuja de abajo a arriba)
     las = 2) # orientación de etquetas (2= siempre perpendicular al eje)

# 3er vector de U (Perfiles de alumnos para el factor 3)
plot(s$u[,3], # posición de cada alumno en la tercera dimensión latente
     ylim = c(-0.5, 0.5))
# 3er vector de V (Pesos de exámenes para el factor 3)
plot(s$v[,3], # agrupa o separa las 24 asignaturas
     ylim = c(-0.5, 0.5))
# dentro del grupo de ciencias, algunos alumnos destacan más en Matemáticas puras y otros en Ciencias naturales

# Reconstrucción de la imagen del 3er componente (artes vacío)
with(s, 
     my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, # genera la matriz de rango 1 de solo la capa 3                t(v[, 3, 
                    drop=FALSE]))) # evita que la matriz de 1 columna sea solo un vector simple
my_image(resid) # comparación con el residuo actual

# ---- Q13: Reconstrucción de Rango 3 y Validación del Modelo de Factores Latentes ----
# la mejor aproximación de rango k para una matriz Y se obtiene utilizando los primeros k valores singulares y sus vectores correspondientes de la SVD
resid <- y - with(s,# matriz original (datos + ruido) - tres primeras columnas (ruido)
                  sweep(u[, 1:3], # selecciona las primeras 3 columnas de la matriz U (perfiles de 100 estudiantes en 3 factores principales)
                        2, # MARGIN: operación aplicada a columnas (1=filas)
                        d[1:3], # 3 primeros valores singulares de cada factor
                        FUN="*") %*% # multiplica cada columna de U por su correspondiente valor singular d
                    t(v[, 1:3])) # traspuesta de las primeras 3 columnas de V (pesos de 24 asignaturas)
my_image(cor(resid), # calcula la correlación entre las columnas de lo que queda (datos - ruido)
         zlim = c(-1,1)) # escala de color (rojo=1, blanco=0, azul=-1)
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# Se ha eliminado la estructura de bloques anterior

y_hat <- with(s, # Y = UkDkVk^(T) para k=3
              sweep(u[, 1:3], # primeras 3 columnas de matriz U
                    2, d[1:3],  # 3 valores más grandes
                    FUN="*") %*% # multiplica cada columna de matriz U xsu valor singular
                t(v[, 1:3])) # toma las primeras 3 columnas de V (pesos de asignaturas) y las transpone para coincidir las dimensiones

# 1. Matriz original
my_image(y, # ruido aleatorio
         zlim = range(y)) # rango de valores q mapean a los colores = límites basados en datos reales

# 2. Visualización del modelo (aproximación) -> versión limpia (sin ruido)
my_image(y_hat, # conservando solo los 3 patrones más importantes
         zlim = range(y)) # mismo significado de colores

# 3. Visualización del error (residuo) -> ruido blanco puro
my_image(y - y_hat, # lo descartado
         zlim = range(y)) # no se puede distinguir qué materia hay en el eje y
# Si ves patrones o bloques en esta imagen, significa que tu modelo de 3 componentes es insuficiente y necesitas un cuarto componente