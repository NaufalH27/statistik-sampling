
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
data$League[data$League == "Serie A 2021/2022"]         <- "Serie A 2021"
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

#membuat wadah untuk hasil sampling
Hasil_sampling =list(EFL2        = dfk,
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
  n = N/(1+N * (0.05)^2) #margin error = 5%
  return (ceiling(n))
}

#algoritma untuk memasukkan data ke cluster yang sesuai sekaligus mengacak/merandom sample untuk setiap cluster
i = 1
while (i <= length(list_cluster)){ #while loop untuk memasukkan pertandingan ke cluster liga
  list_cluster[[i]] <-  data[data$League %in% list_liga[i],]
  
  #membaut randomizer
  baris = nrow(list_cluster[[i]])/2
  randomizer <- sample(1:baris, yamani(baris), replace = FALSE) 
  randomizer = sort(randomizer)
  j = 1
  while (j <= length(randomizer)){ #while loop ke 2 untuk mengacak/merandom pertandingan untuk sample yang mau di ambil
    var = randomizer[j]*2  
    Hasil_sampling[[i]] <- rbind(Hasil_sampling[[i]], list_cluster[[i]][var-1, ])
    Hasil_sampling[[i]] <- rbind(Hasil_sampling[[i]],list_cluster[[i]][var, ])
    j = j+1
  }
  i= i+1
}

#membuat file excel untuk setiap hasil sampling
library(writexl)
k = 1
while (k <= length(Hasil_sampling)){
  nama_file = paste("C:\\Users\\naufal\\Hasil Sampling Statistika\\Hasil Sampling Stratified\\Hasil Sampling ",list_liga[k], ".xlsx", sep = "")
  write_xlsx(Hasil_sampling[[k]], path = nama_file)
  k = k+1
}
