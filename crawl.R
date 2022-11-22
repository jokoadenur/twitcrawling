#Aktivasi Package rtweet
library(rtweet)

#Membuat Kredensial Akses Twitter dengan API
create_token(app = "JokoAde", consumer_key = "xxxxxxxxxxxxxxxxxxxxxxxxxxx",
             consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
             access_token = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
             access_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")

#Crawling data yang dibutuhkan
bbm <- search_tweets(q = "harga bbm", n = 5000, lang = "id", retryonratelimit = F)

#Mengambil data yang tidak terduplikat
nodup <- bbm[!duplicated(bbm$text),]

#Preprocessing data
tweets <- nodup$text %>% as.character()
head(tweets)

#Menghapus karakter \n dalam kalimat
tweets <- gsub( "\n"," ",tweets)
head(tweets)

#Menghapus karakter \r dalam kalimat
tweets <- gsub( "\r"," ",tweets)
head(tweets)

#Menghapus karakter \r dalam kalimat
tweets <- gsub( "[-]"," ",tweets)
head(tweets)

#Mengganti alamat html dan URL dengan blank
library(textclean)
tweets <- tweets %>%
  replace_html() %>%
  replace_url()
head(tweets)

#Mengganti emoji dalam kalimat
tweets <- replace_html(replace_emoji(tweets))
head(tweets)

#Mengganti karakter titik dalam kalimat dengan blank
tweets <- gsub("[.]", "", tweets)
head(tweets)

#Mengganti karakter koma dalam kalimat dengan blank
tweets <- gsub("[,]", "", tweets)
head(tweets)

#Mengganti karakter : dalam kalimat dengan blank
tweets <- gsub("[:]", "", tweets)
head(tweets)

#Mengganti karakter / dalam kalimat dengan blank
tweets <- gsub("/", "", tweets)
head(tweets)

#Mengganti karakter ? dalam kalimat dengan blank
tweets <- gsub("[?]", "", tweets)
head(tweets)

#Mengganti angka dalam kalimat dengan blank (bila perlu)
tweets <- gsub("[[:digit:]]", "", tweets)
head(tweets)

#Menghapus mention dan hashtags (kecuali bila ingin analisis hashtags)
tweets <- gsub("([@#][A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\\w+://S+)", "", tweets)
head(tweets)

#Mengganti kata-kata tertentu (bila ada atau dibutuhkan)
#Misal kata "jln" menjadi "jalan"
tweets <- gsub("yg", "", tweets)
head(tweets, 20)

#Merapikan kalimat dengan strip()
#Penggunaan fungsi strip() juga bisa menghapus angka, titik, koma,!, @, #, ?, \n \r \t dan lainnya
#Membuat teks menjadi lower atau huruf kecil semua
#Fungsi strip() tidak bisa menghapus mention, URL link atau emoji serta karakter "\"
tweets <- strip(tweets)
head(tweets)

#Stemming Bahasa Indonesia
#Melakukan stemming kata dari kata berimbuhan menjadi kata dasar
tweetschar <- as.character(tweets)

library(katadasaR)
stemming <- function(x){
  paste(lapply(x,katadasar),collapse = " ")
}

library(tokenizers)
hasilstem <- lapply(tokenize_words(tweetschar), stemming)
tweets_stopid <- as.character(unlist(hasilstem))
head(tweets_stopid)

#Menghapus duplikat teks lagi pada posisi teks sudah bersih
tweets_stopid <- data.frame(text=unlist(sapply(tweets_stopid, `[`)), stringsAsFactors=F)
tweets_stopid <- data.frame(nodup, tweets_stopid)
nodup <- tweets_stopid[!duplicated(tweets_stopid$text.1),]

#Membersihkan teks kembali
tweets_stopid <- as.character(nodup$text.1)

#Menghapus Teks atau karakter yang hanya tersisa 1 huruf saja
tweets_stopid <- gsub("\\b(.)\\b", " ", tweets_stopid)
head(tweets_stopid)

#Menghapus karakter yang tidak diperlukan
tweets_stopid <- gsub("fav", " ", tweets_stopid)
head(tweets_stopid)

#Membuat Corpus Vektorisasi seluruh teks
library(tm)
docs <- Corpus(VectorSource(tweets_stopid))

#Menghapus keyword dan kata-kata yang tidak diinginkan
#Cek keyword dulu agar independen terhadap analisis
#Cek konjungsi dan partikel lainnya yang tidak perlu
#Hapus juga nama orang bila ada
docs <- tm_map(docs, removeWords, c('harga','bbm','ini','dan','padahal','akan', 'yg','akn','ttg','pdhl',
                                    'thd','itu','juga','jg','dr', 'dari', 'pada','tentang','klo','kalau',
                                    'bahwa','yg','disana','disitu','ttg',
                                    'oleh','olh','adalah','adl','di situ',
                                    'tetapi','tapi','namun','melainkan','lalu','serta','dengan','dgn',
                                    'meski','meskipun','demikian','karena','krn','nih','tuh',
                                    'kalo','orang','saja','aja','walau','walaupun','daripada','gue','gua','gtu',
                                    'iya','gais','emng','and','emg','gitu','hai','hallo','haloo','hrs','gimana',
                                    'gengs','ges','hai','hii','halo','dri','dsb','dll','dkk','dlm','dpt'))

#Menghapus kata dengan Stopwords
stop <- readLines("~/stopwords-indonesia.txt")
docs <- tm_map(docs, removeWords, stop)
inspect(docs)

#Mengubah menjadi data frame kembali dan disimpan
dataframe<-data.frame(text=unlist(sapply(docs, `[`)), stringsAsFactors=F)

#Pelabelan Sentimen
#Menentukan Skor Sentimen
kalimat2 <- dataframe

positif <- scan("se-pos.txt",what="character",comment.char=";")
negatif <- scan("se-neg.txt",what="character",comment.char=";")
kata.positif = c(positif)
kata.negatif = c(negatif)
score.sentiment = function(kalimat2, kata.positif, kata.negatif, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(kalimat2, function(kalimat, kata.positif, kata.negatif) {
    list.kata = str_split(kalimat, '\\s+')
    kata2 = unlist(list.kata)
    positif.matches = match(kata2, kata.positif)
    negatif.matches = match(kata2, kata.negatif)
    positif.matches = !is.na(positif.matches)
    negatif.matches = !is.na(negatif.matches)
    score = sum(positif.matches) - (sum(negatif.matches))
    return(score)
  }, kata.positif, kata.negatif, .progress=.progress )
  scores.df = data.frame(score=scores, text=kalimat2)
  return(scores.df)
}

hasil = score.sentiment(kalimat2$text, kata.positif, kata.negatif)
head(hasil)

#Konversi skor ke kategori sentimen
hasil$klasifikasi <- ifelse(hasil$score < 0, "Negatif", ifelse(hasil$score > 0, "Positif", "Netral"))
Kategori_Sentimen_2 <- cbind(hasil$klasifikasi)
table(Kategori_Sentimen_2)

#Gabung dengan data no duplikasi
gabung <- data.frame(nodup, hasil, stringsAsFactors = F)
library(openxlsx)
write.xlsx(gabung, "bbmdataset.xlsx")
