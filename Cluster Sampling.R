

#membaca file yang berisi kumpulan data pertandingan yang di catat oleh kelas C dan D
getwd()
data <- read.csv("league.csv", sep = ";")

#data cleaning, menyamakan nama seperti perbedaan huruf kapital, spasi, dan lain lain
liga<- unique(data$League)
list_liga = lapply(liga, function(x) unlist(strsplit(x, "\"  \"")))
data$League[data$League == "English football league 2"] <- "English Football League 2"
data$League[data$League == "J1"]                        <- "J1 League"
data$League[data$League == "J1 League "]                <- "J1 League"
data$League[data$League == "ligue1"]                    <- "Ligue 1"
data$League[data$League == "League 1"]                  <- "Ligue 1"
data$League[data$League == "Bundesliga"]                <- "bundesliga"
data$League[data$League == "Serie A "]                  <- "Serie A"
liga<- unique(data$League)
list_liga = lapply(liga, function(x) unlist(strsplit(x, "\"  \"")))

#menggrupkan setiap pertandingan ke sebuah cluster sesuai liga-liga pertandingan tersebut
##membuat tabel kosong
dfk <- data.frame(matrix(nrow = 0, ncol = ncol(data)))
colnames(dfk) <- colnames(data)

##mempersiapkan wadah untuk di masukkan pertandingan sesuai liganya, dengan mendeklarasikan setiap element menjadi tabel kosong
list_cluster = list(EFL2        = dfk,
                    MLS         = dfk,
                    La_Liga     = dfk,
                    Serie_A     = dfk,
                    Allsvenskan = dfk,
                    J1          = dfk,
                    Eredevisie  = dfk,
                    EPL         = dfk,
                    bundesliga  = dfk,
                    SPL         = dfk,
                    Ligue_1     = dfk,
                    La_Liga_22  = dfk,
                    CBS_A       = dfk,
                    P_League    = dfk,
                    Serie_A_22  = dfk,
                    APD         = dfk,
                    Liga_super  = dfk)

#membuat fungsi untuk sample size menggunakan metode yamani
yamani <- function(N){
  n = N/(1+N * (0.5)^2) #margin error = 50%
  return (ceiling(n))
}

#membuat wadah untuk hasil sampling
hasil_sampling = dfk

#algoritma untuk memasukkan data ke cluster yang sesuai sekaligus mengacak/merandom sample untuk setiap cluster
i = 1
while (i <= length(list_cluster)){ #while loop untuk memasukkan pertandingan ke cluster sesuai liga masing masing
  list_cluster[[i]] <-  data[data$League %in% list_liga[i],]
  i= i+1
}

#merandom cluster yang mau dipilih
jumlah_cluster = length(list_cluster)
randomizer <- sample(1:jumlah_cluster,yamani(jumlah_cluster)) 
randomizer <- sort(randomizer)
for (j in randomizer){ #for loop untuk mengacak/merandom liga yang mau di pilih sebagai sample
  hasil_sampling <- rbind(hasil_sampling, list_cluster[[j]])
  j = j+1
}

#mencetak file excel untuk hasil samplingnya
library(writexl)
write_xlsx(hasil_sampling, path = "C:\\Users\\naufal\\Hasil Sampling Statistika\\Hasil Sampling Cluster Method.xlsx")


