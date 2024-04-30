#membaca file yang berisi kumpulan data pertandingan yang di catat oleh kelas C dan D
getwd()
data = read.csv("league.csv", sep = ";")

#membuat fungsi untuk sample size menggunakan metode yamani
yamani <- function(N){
  n = N/(1+N * (0.05)^2) #margin error = 5%
  return (ceiling(n))
}

#membuat systematic sampling
interval = (nrow(data)/2)/(yamani(nrow(data)/2))
systematic_sampling = round(seq(1,length(data$Team)/2,interval))

#membuat tabel kosong sebagai wadah untuk meletakkan hasil sample
tabel_hasil_sampling <- data.frame(matrix(nrow = 0, ncol = ncol(data)))
colnames(tabel_hasil_sampling) <- colnames(data)

#while loop untuk memasukkan pertandingan hasil systematic ke dalam tabel hasil sampling
i = 1
while (i <= length(systematic_sampling)){
  var = systematic_sampling[i]*2
  tabel_hasil_sampling <- rbind(tabel_hasil_sampling, data[var-1, ] )
  tabel_hasil_sampling <- rbind(tabel_hasil_sampling, data[var, ])
  i = i+1
}

#membuat file excel untuk mencetak hasil sampling 
install.packages("writexl")
library(writexl)
write_xlsx(tabel_hasil_sampling, path = "C:\\Users\\naufal\\Hasil Sampling Statistika\\Hasil Sampling Systematic Method.xlsx")
