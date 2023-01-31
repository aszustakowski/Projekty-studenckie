lipiec2017<-read.csv("C:\\Users\\pc\\Desktop\\pdu3dane\\lipiec2017.csv")
lipiec2018<-read.csv("C:\\Users\\pc\\Desktop\\pdu3dane\\lipiec2018.csv")
czerwiec2019<-read.csv("C:\\Users\\pc\\Desktop\\pdu3dane\\czerwiec2019.csv")
lipiec2019<-read.csv("C:\\Users\\pc\\Desktop\\pdu3dane\\lipiec2019.csv")
sierpien2019<-read.csv("C:\\Users\\pc\\Desktop\\pdu3dane\\sierpien2019.csv")
wakacje2019<-rbind(czerwiec2019, lipiec2019, sierpien2019)
srednia_dni_tygodnia<-function(){
  czerwiec2019$Data <- sapply(strsplit(as.character(czerwiec2019$starttime), " "), "[", 1)
  czerwiec2019$Czas <- sapply(strsplit(as.character(czerwiec2019$starttime), " "), "[", 2)
  czerwiec2019$DataKonca <- sapply(strsplit(as.character(czerwiec2019$stoptime), " "), "[", 1)
  czerwiec2019$czasKonca <- sapply(strsplit(as.character(czerwiec2019$stoptime), " "), "[", 2)
  lipiec2019$Data <- sapply(strsplit(as.character(lipiec2019$starttime), " "), "[", 1)
  lipiec2019$Czas <- sapply(strsplit(as.character(lipiec2019$starttime), " "), "[", 2)
  lipiec2019$DataKonca <- sapply(strsplit(as.character(lipiec2019$stoptime), " "), "[", 1)
  lipiec2019$czasKonca <- sapply(strsplit(as.character(lipiec2019$stoptime), " "), "[", 2)
  sierpien2019$Data <- sapply(strsplit(as.character(sierpien2019$starttime), " "), "[", 1)
  sierpien2019$Czas <- sapply(strsplit(as.character(sierpien2019$starttime), " "), "[", 2)
  sierpien2019$DataKonca <- sapply(strsplit(as.character(sierpien2019$stoptime), " "), "[", 1)
  sierpien2019$czasKonca <- sapply(strsplit(as.character(sierpien2019$stoptime), " "), "[", 2)
  czerwiec2019_poniedzialek<-subset(czerwiec2019,
                                    Data=="2019-06-03" | Data=="2019-06-10" | Data=="2019-06-17" | Data=="2019-06-24")
  czerwiec2019_wtorek<-subset(czerwiec2019,
                               Data=="2019-06-04" | Data=="2019-06-11" | Data=="2019-06-18" | Data=="2019-06-25")
  czerwiec2019_sroda<-subset(czerwiec2019,
                             Data=="2019-06-05" | Data=="2019-06-12" | Data=="2019-06-19" | Data=="2019-06-26")
  czerwiec2019_czwartek<-subset(czerwiec2019,
                                Data=="2019-06-06" | Data=="2019-06-13" | Data=="2019-06-20" | Data=="2019-06-27")
  czerwiec2019_piatek<-subset(czerwiec2019,
                              Data=="2019-06-07" | Data=="2019-06-14" | Data=="2019-06-21" | Data=="2019-06-28")
  czerwiec2019_sobota<-subset(czerwiec2019, Data=="2019-06-01" |
                            Data=="2019-06-08" | Data=="2019-06-15" | Data=="2019-06-22" | Data=="2019-06-29")
  czerwiec2019_niedziela<-subset(czerwiec2019, Data=="2019-06-02" |
                                 Data=="2019-06-09" | Data=="2019-06-16" | Data=="2019-06-23" | Data=="2019-06-30")
  lipiec2019_poniedzialek<-subset(lipiec2019, Data=="2019-07-01" | Data=="2019-07-08" | Data=="2019-07-15"
                                  | Data=="2019-07-22" | Data=="2019-07-29")
  lipiec2019_wtorek<-subset(lipiec2019, Data=="2019-07-02" | Data=="2019-07-09" | Data=="2019-07-16"
                                  | Data=="2019-07-23" | Data=="2019-07-30")
  lipiec2019_sroda<-subset(lipiec2019, Data=="2019-07-03" | Data=="2019-07-10" | Data=="2019-07-17"
                                  | Data=="2019-07-24" | Data=="2019-07-31")
  lipiec2019_czwartek<-subset(lipiec2019, Data=="2019-07-04" | Data=="2019-07-11" | Data=="2019-07-18"
                                  | Data=="2019-07-25")
  lipiec2019_piatek<-subset(lipiec2019, Data=="2019-07-05" | Data=="2019-07-12" | Data=="2019-07-19"
                              | Data=="2019-07-26")
  lipiec2019_sobota<-subset(lipiec2019, Data=="2019-07-06" | Data=="2019-07-13" | Data=="2019-07-20"
                              | Data=="2019-07-27")
  lipiec2019_niedziela<-subset(lipiec2019, Data=="2019-07-07" | Data=="2019-07-14" | Data=="2019-07-21"
                              | Data=="2019-07-28")
  sierpien2019_poniedzialek<-subset(sierpien2019, Data=="2019-08-05" | Data=="2019-08-12" | Data=="2019-08-19"
                                    | Data=="2019-08-26")
  sierpien2019_wtorek<-subset(sierpien2019, Data=="2019-08-06" | Data=="2019-08-13" | Data=="2019-08-20"
                                    | Data=="2019-08-27")
  sierpien2019_sroda<-subset(sierpien2019, Data=="2019-08-07" | Data=="2019-08-14" | Data=="2019-08-21"
                                    | Data=="2019-08-28")
  sierpien2019_czwartek<-subset(sierpien2019,Data=="2019-08-01" | Data=="2019-08-08" | Data=="2019-08-15" | 
                                      Data=="2019-08-22" | Data=="2019-08-29")
  sierpien2019_piatek<-subset(sierpien2019,Data=="2019-08-02" | Data=="2019-08-09" | Data=="2019-08-16" | 
                                  Data=="2019-08-23" | Data=="2019-08-30")
  sierpien2019_sobota<-subset(sierpien2019,Data=="2019-08-03" | Data=="2019-08-10" | Data=="2019-08-17" | 
                                  Data=="2019-08-24" | Data=="2019-08-31")
  sierpien2019_niedziela<-subset(sierpien2019,Data=="2019-08-04" | Data=="2019-08-11" | Data=="2019-08-18" | 
                                  Data=="2019-08-25")
  suma_3_miesiace<-nrow(czerwiec2019)+nrow(lipiec2019)+nrow(sierpien2019)
  suma_poniedzialek<-nrow(czerwiec2019_poniedzialek)+nrow(lipiec2019_poniedzialek)+nrow(sierpien2019_poniedzialek)
  suma_wtorek<-nrow(czerwiec2019_wtorek)+nrow(lipiec2019_wtorek)+nrow(sierpien2019_wtorek)
  suma_sroda<-nrow(czerwiec2019_sroda)+nrow(lipiec2019_sroda)+nrow(sierpien2019_sroda)
  suma_czwartek<-nrow(czerwiec2019_czwartek)+nrow(lipiec2019_czwartek)+nrow(sierpien2019_czwartek)
  suma_piatek<-nrow(czerwiec2019_piatek)+nrow(lipiec2019_piatek)+nrow(sierpien2019_piatek)
  suma_sobota<-nrow(czerwiec2019_sobota)+nrow(lipiec2019_sobota)+nrow(sierpien2019_sobota)
  suma_niedziela<-nrow(czerwiec2019_niedziela)+nrow(lipiec2019_niedziela)+nrow(sierpien2019_niedziela)
  procenty_poniedzialek<-round(100*suma_poniedzialek/suma_3_miesiace, digits = 1)
  procenty_wtorek<-round(100*suma_wtorek/suma_3_miesiace, digits = 1)
  procenty_sroda<-round(100*suma_sroda/suma_3_miesiace, digits = 1)
  procenty_czwartek<-round(100*suma_czwartek/suma_3_miesiace, digits = 1)
  procenty_piatek<-round(100*suma_piatek/suma_3_miesiace, digits = 1)
  procenty_sobota<-round(100*suma_sobota/suma_3_miesiace, digits = 1)
  procenty_niedziela<-round(100*suma_niedziela/suma_3_miesiace, digits = 1)
  dzien_tygodnia<-c("poniedzialek", "wtorek", "sroda", "czwartek", "piatek", "sobota", "niedziela")
  udzial_w_procentach<-c(procenty_poniedzialek, procenty_wtorek, procenty_sroda, procenty_czwartek,
                          procenty_piatek, procenty_sobota, procenty_niedziela)
  dzien_tygodnia<-paste(dzien_tygodnia, udzial_w_procentach, sep= " - ")
  dzien_tygodnia<-paste(dzien_tygodnia, "%", sep = "")
  pie(udzial_w_procentach, labels = dzien_tygodnia,col = rainbow(length(dzien_tygodnia)), main = "Udzia³ procentowy dni tygodnia")
}
czwarty_lipca_stacja<-function(){
  lipiec2017$DataStartu <- sapply(strsplit(as.character(lipiec2017$starttime), " "), "[", 1)
  lipiec2017$CzasStartu <- sapply(strsplit(as.character(lipiec2017$starttime), " "), "[", 2)
  lipiec2017$DataKonca <- sapply(strsplit(as.character(lipiec2017$stoptime), " "), "[", 1)
  lipiec2017$CzasKonca <- sapply(strsplit(as.character(lipiec2017$stoptime), " "), "[", 2)
  lipiec2018$DataStartu <- sapply(strsplit(as.character(lipiec2018$starttime), " "), "[", 1)
  lipiec2018$CzasStartu <- sapply(strsplit(as.character(lipiec2018$starttime), " "), "[", 2)
  lipiec2018$DataKonca <- sapply(strsplit(as.character(lipiec2018$stoptime), " "), "[", 1)
  lipiec2018$CzasKonca <- sapply(strsplit(as.character(lipiec2018$stoptime), " "), "[", 2)
  lipiec2019$DataStartu <- sapply(strsplit(as.character(lipiec2019$starttime), " "), "[", 1)
  lipiec2019$CzasStartu <- sapply(strsplit(as.character(lipiec2019$starttime), " "), "[", 2)
  lipiec2019$DataKonca <- sapply(strsplit(as.character(lipiec2019$stoptime), " "), "[", 1)
  lipiec2019$CzasKonca <- sapply(strsplit(as.character(lipiec2019$stoptime), " "), "[", 2)
  lipiec2017_czwarty_lipca<-subset(lipiec2017, DataKonca=="2017-07-04")
  lipiec2018_czwarty_lipca<-subset(lipiec2018, DataKonca=="2018-07-04")
  lipiec2019_czwarty_lipca<-subset(lipiec2019, DataKonca=="2019-07-04")
  Stacje_17 <- aggregate(x=lipiec2017_czwarty_lipca$end.station.name, by= lipiec2017_czwarty_lipca['end.station.name']
                         ,FUN= length)
  names(Stacje_17)[2]<-"2017"
  Stacje_18 <- aggregate(x=lipiec2018_czwarty_lipca$end.station.name, by= lipiec2018_czwarty_lipca['end.station.name']
                         ,FUN= length)
  names(Stacje_18)[2]<-"2018"
  Stacje_19 <- aggregate(x=lipiec2019_czwarty_lipca$end.station.name, by= lipiec2019_czwarty_lipca['end.station.name']
                         ,FUN= length)
  names(Stacje_19)[2]<-"2019"
  Stacje_17_18<-merge(Stacje_17, Stacje_18, by="end.station.name")
  Stacje_17_18_19<-merge(Stacje_17_18, Stacje_19, by="end.station.name")
  Stacje_17_18_19<-subset(Stacje_17_18_19, Stacje_17_18_19[2]>35 & Stacje_17_18_19[3]>35 & Stacje_17_18_19[4]>35)
  row.names(Stacje_17_18_19)<-NULL
  names(Stacje_17_18_19)[1]<-"Nazwa stacji koñcowej"
  Stacje_17_18_19
}
customer_udzial<-function(){
  podzial_czerwiec2019<-aggregate(x=czerwiec2019$usertype, by=czerwiec2019["usertype"], FUN= length)
  podzial_lipiec2019<-aggregate(x=lipiec2019$usertype, by=lipiec2019["usertype"], FUN= length)
  podzial_sierpien2019<-aggregate(x=sierpien2019$usertype, by=sierpien2019["usertype"], FUN= length)
  laczny_podzial_1<-merge(podzial_czerwiec2019, podzial_lipiec2019, by="usertype")
  laczny_podzial_2<-merge(laczny_podzial_1, podzial_sierpien2019, by="usertype")
  przejazdy_customer<-laczny_podzial_2$x.x[1]+laczny_podzial_2$x.y[1]+laczny_podzial_2$x[1]
  przejazdy_subscriber<-laczny_podzial_2$x.x[2]+laczny_podzial_2$x.y[2]+laczny_podzial_2$x[2]
  laczne_przejazdy<-przejazdy_customer+przejazdy_subscriber
  podzial_wakacje<-data.frame(
    Rodzaj_u¿ytkownika=c("Customer", "Subscriber"),
    Liczba_przejazdów=c(przejazdy_customer, przejazdy_subscriber))
  ggplot(podzial_wakacje, aes(Rodzaj_u¿ytkownika, Liczba_przejazdów)) + geom_col(fill="blue")
}
customer_analiza<-function(){
  customer_subset_czerwiec2019<-subset(czerwiec2019, usertype=="Customer")
  customer_subset_lipiec2019<-subset(lipiec2019, usertype=="Customer")
  customer_subset_sierpien2019<-subset(sierpien2019, usertype=="Customer")
  czerwieclipiec_customer<-rbind(customer_subset_czerwiec2019, customer_subset_lipiec2019)
  customer_wakacje<-rbind(czerwieclipiec_customer, customer_subset_sierpien2019)
  row.names(customer_wakacje)<-NULL
  customer_16_25_0<-subset(customer_wakacje,
                          birth.year<=2003 & birth.year>=1994 & gender==0)
  customer_16_25_1<-subset(customer_wakacje,
                           birth.year<=2003 & birth.year>=1994 & gender==1)
  customer_16_25_2<-subset(customer_wakacje,
                           birth.year<=2003 & birth.year>=1994 & gender==2)
  customer_26_35_0<-subset(customer_wakacje,
                          birth.year<1994 & birth.year>=1984 & gender==0)
  customer_26_35_1<-subset(customer_wakacje,
                           birth.year<1994 & birth.year>=1984 & gender==1)
  customer_26_35_2<-subset(customer_wakacje,
                           birth.year<1994 & birth.year>=1984 & gender==2)
  customer_36_45_0<-subset(customer_wakacje,
                          birth.year<1984 & birth.year>=1974 & gender==0)
  customer_36_45_1<-subset(customer_wakacje,
                           birth.year<1984 & birth.year>=1974 & gender==1)
  customer_36_45_2<-subset(customer_wakacje,
                           birth.year<1984 & birth.year>=1974 & gender==2)
  customer_46_55_0<-subset(customer_wakacje,
                          birth.year<1974 & birth.year>=1964 & gender==0)
  customer_46_55_1<-subset(customer_wakacje,
                           birth.year<1974 & birth.year>=1964 & gender==1)
  customer_46_55_2<-subset(customer_wakacje,
                           birth.year<1974 & birth.year>=1964 & gender==2)
  customer_56_65_0<-subset(customer_wakacje,
                          birth.year<1964 & birth.year>=1954 & gender==0)
  customer_56_65_1<-subset(customer_wakacje,
                           birth.year<1964 & birth.year>=1954 & gender==1)
  customer_56_65_2<-subset(customer_wakacje,
                           birth.year<1964 & birth.year>=1954 & gender==2)
  customer_66_0<-subset(customer_wakacje,
                       birth.year<1954 & birth.year>1920 & gender==0)
  customer_66_1<-subset(customer_wakacje,
                        birth.year<1954 & birth.year>1920 & gender==1)
  customer_66_2<-subset(customer_wakacje,
                        birth.year<1954 & birth.year>1920 & gender==2)
  œrednia_16_25_0<-round(mean(customer_16_25_0[["tripduration"]])/60, digits = 0)
  œrednia_16_25_1<-round(mean(customer_16_25_1[["tripduration"]])/60, digits = 0)
  œrednia_16_25_2<-round(mean(customer_16_25_2[["tripduration"]])/60, digits = 0)
  œrednia_26_35_0<-round(mean(customer_26_35_0[["tripduration"]])/60, digits = 0)
  œrednia_26_35_1<-round(mean(customer_26_35_1[["tripduration"]])/60, digits = 0)
  œrednia_26_35_2<-round(mean(customer_26_35_2[["tripduration"]])/60, digits = 0)
  œrednia_36_45_0<-round(mean(customer_36_45_0[["tripduration"]])/60, digits = 0)
  œrednia_36_45_1<-round(mean(customer_36_45_1[["tripduration"]])/60, digits = 0)
  œrednia_36_45_2<-round(mean(customer_36_45_2[["tripduration"]])/60, digits = 0)
  œrednia_46_55_0<-round(mean(customer_46_55_0[["tripduration"]])/60, digits = 0)
  œrednia_46_55_1<-round(mean(customer_46_55_1[["tripduration"]])/60, digits = 0)
  œrednia_46_55_2<-round(mean(customer_46_55_2[["tripduration"]])/60, digits = 0)
  œrednia_56_65_0<-"brak przejazdów"
  œrednia_56_65_1<-round(mean(customer_56_65_1[["tripduration"]])/60, digits = 0)
  œrednia_56_65_2<-round(mean(customer_56_65_2[["tripduration"]])/60, digits = 0)
  œrednia_66_0<-"brak przejazdów"
  œrednia_66_1<-round(mean(customer_66_1[["tripduration"]])/60, digits = 0)
  œrednia_66_2<-round(mean(customer_66_2[["tripduration"]])/60, digits = 0)
  przejazdy_16_25_0<-nrow(customer_16_25_0)
  przejazdy_16_25_1<-nrow(customer_16_25_1)
  przejazdy_16_25_2<-nrow(customer_16_25_2)
  przejazdy_26_35_0<-nrow(customer_26_35_0)
  przejazdy_26_35_1<-nrow(customer_26_35_1)
  przejazdy_26_35_2<-nrow(customer_26_35_2)
  przejazdy_36_45_0<-nrow(customer_36_45_0)
  przejazdy_36_45_1<-nrow(customer_36_45_1)
  przejazdy_36_45_2<-nrow(customer_36_45_2)
  przejazdy_46_55_0<-nrow(customer_46_55_0)
  przejazdy_46_55_1<-nrow(customer_46_55_1)
  przejazdy_46_55_2<-nrow(customer_46_55_2)
  przejazdy_56_65_0<-nrow(customer_56_65_0)
  przejazdy_56_65_1<-nrow(customer_56_65_1)
  przejazdy_56_65_2<-nrow(customer_56_65_2)
  przejazdy_66_0<-nrow(customer_66_0)
  przejazdy_66_1<-nrow(customer_66_1)
  przejazdy_66_2<-nrow(customer_66_2)
  analiza_customer<-data.frame(
    wiek=c("16-25", "16-25", "16-25", "26-35", "26-35", "26-35","36-45","36-45","36-45",
           "46-55","46-55","46-55","56-65","56-65","56-65","66+","66+","66+"),
    p³eæ=c("nie podano", "mê¿czyzna", "kobieta"),
    przejazdy=c(przejazdy_16_25_0, przejazdy_16_25_1, przejazdy_16_25_2, przejazdy_26_35_0, przejazdy_26_35_1
                        ,przejazdy_26_35_2, przejazdy_36_45_0, przejazdy_36_45_1, przejazdy_36_45_2, przejazdy_46_55_0
                        ,przejazdy_46_55_1, przejazdy_46_55_2, przejazdy_56_65_0, przejazdy_56_65_1,
                        przejazdy_56_65_2, przejazdy_66_0, przejazdy_66_1, przejazdy_66_2),
    czas_jazdy=c(œrednia_16_25_0, œrednia_16_25_1, œrednia_16_25_2, œrednia_26_35_0, œrednia_26_35_1,
                         œrednia_26_35_2, œrednia_36_45_0, œrednia_36_45_1, œrednia_36_45_2, œrednia_46_55_0,
                         œrednia_46_55_1, œrednia_46_55_2, œrednia_56_65_0, œrednia_56_65_1, œrednia_56_65_2,
                         œrednia_66_0, œrednia_66_1, œrednia_66_2)
  )
  analiza_customer
}
najlepsze_godziny<-function(){
  wakacje_godziny<-rbind(czerwiec2019, lipiec2019, sierpien2019)
  wakacje_godziny$DataStartu <- sapply(strsplit(as.character(wakacje_godziny$starttime), " "), "[", 1)
  wakacje_godziny$CzasStartu <- sapply(strsplit(as.character(wakacje_godziny$starttime), " "), "[", 2)
  wakacje_godziny$DataKonca <- sapply(strsplit(as.character(wakacje_godziny$stoptime), " "), "[", 1)
  wakacje_godziny$CzasKonca <- sapply(strsplit(as.character(wakacje_godziny$stoptime), " "), "[", 2)
  wakacje_godziny$starttime<-NULL
  wakacje_godziny$stoptime<-NULL
  wakacje_godziny$CzasStartu<-strptime(wakacje_godziny$CzasStartu, format = "%H:%M:%OS")
  wakacje_godziny$CzasKonca<-strptime(wakacje_godziny$CzasKonca, format = "%H:%M:%OS")
  godziny_0_2<-subset(wakacje_godziny, CzasStartu>=strptime("00:00:00", format = "%H:%M:%S") & CzasKonca<strptime("02:00:00", format = "%H:%M:%S"))
  godziny_2_4<-subset(wakacje_godziny, CzasStartu>=strptime("02:00:00", format = "%H:%M:%S") & CzasKonca<strptime("04:00:00", format = "%H:%M:%S"))
  godziny_4_6<-subset(wakacje_godziny, CzasStartu>=strptime("04:00:00", format = "%H:%M:%S") & CzasKonca<strptime("06:00:00", format = "%H:%M:%S"))
  godziny_6_8<-subset(wakacje_godziny, CzasStartu>=strptime("06:00:00", format = "%H:%M:%S") & CzasKonca<strptime("08:00:00", format = "%H:%M:%S"))
  godziny_8_10<-subset(wakacje_godziny, CzasStartu>=strptime("08:00:00", format = "%H:%M:%S") & CzasKonca<strptime("10:00:00", format = "%H:%M:%S"))
  godziny_10_12<-subset(wakacje_godziny, CzasStartu>=strptime("10:00:00", format = "%H:%M:%S") & CzasKonca<strptime("12:00:00", format = "%H:%M:%S"))
  godziny_12_14<-subset(wakacje_godziny, CzasStartu>=strptime("12:00:00", format = "%H:%M:%S") & CzasKonca<strptime("14:00:00", format = "%H:%M:%S"))
  godziny_14_16<-subset(wakacje_godziny, CzasStartu>=strptime("14:00:00", format = "%H:%M:%S") & CzasKonca<strptime("16:00:00", format = "%H:%M:%S"))
  godziny_16_18<-subset(wakacje_godziny, CzasStartu>=strptime("16:00:00", format = "%H:%M:%S") & CzasKonca<strptime("18:00:00", format = "%H:%M:%S"))
  godziny_18_20<-subset(wakacje_godziny, CzasStartu>=strptime("18:00:00", format = "%H:%M:%S") & CzasKonca<strptime("20:00:00", format = "%H:%M:%S"))
  godziny_20_22<-subset(wakacje_godziny, CzasStartu>=strptime("20:00:00", format = "%H:%M:%S") & CzasKonca<strptime("22:00:00", format = "%H:%M:%S"))
  godziny_22_24<-subset(wakacje_godziny, CzasStartu>=strptime("22:00:00", format = "%H:%M:%S") & CzasKonca<strptime("24:00:00", format = "%H:%M:%S"))
  suma_godzin<-nrow(wakacje_godziny)
  popularne_godziny<-data.frame(
    godzina_startu=c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23),
    procentowy_udzia³=c(round(100*nrow(godziny_0_2)/suma_godzin, digits = 2), round(100*nrow(godziny_2_4)/suma_godzin, digits = 2), round(100*nrow(godziny_4_6)/suma_godzin, digits = 2),
                         round(100*nrow(godziny_6_8)/suma_godzin, digits = 2), round(100*nrow(godziny_8_10)/suma_godzin, digits = 2), round(100*nrow(godziny_10_12)/suma_godzin, digits = 2),
                         round(100*nrow(godziny_12_14)/suma_godzin, digits = 2), round(100*nrow(godziny_14_16)/suma_godzin, digits = 2), round(100*nrow(godziny_16_18)/suma_godzin, digits = 2),
                         round(100*nrow(godziny_18_20)/suma_godzin, digits = 2), round(100*nrow(godziny_20_22)/suma_godzin, digits = 2), round(100*nrow(godziny_22_24)/suma_godzin, digits = 2))
  )
  ggplot(popularne_godziny, aes(godzina_startu, procentowy_udzia³)) + geom_col(color="red", fill="green")
}
wiek_godziny<-function(){
  wakacje_wiek_godziny<-rbind(czerwiec2019, lipiec2019, sierpien2019)
  wakacje_wiek_godziny$DataStartu <- sapply(strsplit(as.character(wakacje_wiek_godziny$starttime), " "), "[", 1)
  wakacje_wiek_godziny$CzasStartu <- sapply(strsplit(as.character(wakacje_wiek_godziny$starttime), " "), "[", 2)
  wakacje_wiek_godziny$DataKonca <- sapply(strsplit(as.character(wakacje_wiek_godziny$stoptime), " "), "[", 1)
  wakacje_wiek_godziny$CzasKonca <- sapply(strsplit(as.character(wakacje_wiek_godziny$stoptime), " "), "[", 2)
  wakacje_wiek_godziny$starttime<-NULL
  wakacje_wiek_godziny$stoptime<-NULL
  wakacje_wiek_godziny$CzasStartu<-strptime(wakacje_wiek_godziny$CzasStartu, format = "%H:%M:%OS")
  wakacje_wiek_godziny$CzasKonca<-strptime(wakacje_wiek_godziny$CzasKonca, format = "%H:%M:%OS")
  wiek_16_25_8_10<-subset(wakacje_wiek_godziny, CzasStartu>=strptime("08:00:00", format = "%H:%M:%S") &
                            CzasKonca<strptime("10:00:00", format = "%H:%M:%S") & birth.year<=2003 & birth.year>=1994)
  wiek_26_35_8_10<-subset(wakacje_wiek_godziny, CzasStartu>=strptime("08:00:00", format = "%H:%M:%S") &
                            CzasKonca<strptime("10:00:00", format = "%H:%M:%S") & birth.year<1994 & birth.year>=1984)
  wiek_36_45_8_10<-subset(wakacje_wiek_godziny, CzasStartu>=strptime("08:00:00", format = "%H:%M:%S") &
                            CzasKonca<strptime("10:00:00", format = "%H:%M:%S") & birth.year<1984 & birth.year>=1974)
  wiek_46_55_8_10<-subset(wakacje_wiek_godziny, CzasStartu>=strptime("08:00:00", format = "%H:%M:%S") &
                            CzasKonca<strptime("10:00:00", format = "%H:%M:%S") & birth.year<=1974 & birth.year>=1964)
  wiek_56_65_8_10<-subset(wakacje_wiek_godziny, CzasStartu>=strptime("08:00:00", format = "%H:%M:%S") &
                            CzasKonca<strptime("10:00:00", format = "%H:%M:%S") & birth.year<1964 & birth.year>=1954)
  wiek_66_8_10<-subset(wakacje_wiek_godziny, CzasStartu>=strptime("08:00:00", format = "%H:%M:%S") &
                            CzasKonca<strptime("10:00:00", format = "%H:%M:%S") & birth.year<1954 & birth.year>=1930)
  wiek_16_25_18_20<-subset(wakacje_wiek_godziny, CzasStartu>=strptime("18:00:00", format = "%H:%M:%S") &
                            CzasKonca<strptime("20:00:00", format = "%H:%M:%S") & birth.year<=2003 & birth.year>=1994)
  wiek_26_35_18_20<-subset(wakacje_wiek_godziny, CzasStartu>=strptime("18:00:00", format = "%H:%M:%S") &
                            CzasKonca<strptime("20:00:00", format = "%H:%M:%S") & birth.year<1994 & birth.year>=1984)
  wiek_36_45_18_20<-subset(wakacje_wiek_godziny, CzasStartu>=strptime("18:00:00", format = "%H:%M:%S") &
                            CzasKonca<strptime("20:00:00", format = "%H:%M:%S") & birth.year<1984 & birth.year>=1974)
  wiek_46_55_18_20<-subset(wakacje_wiek_godziny, CzasStartu>=strptime("18:00:00", format = "%H:%M:%S") &
                            CzasKonca<strptime("20:00:00", format = "%H:%M:%S") & birth.year<=1974 & birth.year>=1964)
  wiek_56_65_18_20<-subset(wakacje_wiek_godziny, CzasStartu>=strptime("18:00:00", format = "%H:%M:%S") &
                            CzasKonca<strptime("20:00:00", format = "%H:%M:%S") & birth.year<1964 & birth.year>=1954)
  wiek_66_18_20<-subset(wakacje_wiek_godziny, CzasStartu>=strptime("18:00:00", format = "%H:%M:%S") &
                         CzasKonca<strptime("20:00:00", format = "%H:%M:%S") & birth.year<1954 & birth.year>=1930)
  suma_8_10<-nrow(subset(wakacje_wiek_godziny,CzasStartu>=strptime("08:00:00", format = "%H:%M:%S") &
                      CzasKonca<strptime("10:00:00", format = "%H:%M:%S")))
  suma_18_20<-nrow(subset(wakacje_wiek_godziny,CzasStartu>=strptime("18:00:00", format = "%H:%M:%S") &
                           CzasKonca<strptime("20:00:00", format = "%H:%M:%S")))
  godziny_wiek<-data.frame(
    wiek=c("16-25","26-35","36-45","46-55","56-65","66+"),
    godzina=c("8-10","8-10","8-10","8-10","8-10","8-10", "18-20","18-20","18-20","18-20","18-20","18-20"),
    procentowy_udzia³=c(round(100*nrow(wiek_16_25_8_10)/suma_8_10, digits = 2), round(100*nrow(wiek_26_35_8_10)/suma_8_10, digits = 2), round(100*nrow(wiek_36_45_8_10)/suma_8_10, digits = 2),
                        round(100*nrow(wiek_46_55_8_10)/suma_8_10, digits = 2), round(100*nrow(wiek_56_65_8_10)/suma_8_10, digits = 2), round(100*nrow(wiek_66_8_10)/suma_8_10, digits = 2),
                        round(100*nrow(wiek_16_25_18_20)/suma_18_20, digits = 2), round(100*nrow(wiek_26_35_18_20)/suma_18_20, digits = 2), round(100*nrow(wiek_36_45_18_20)/suma_18_20, digits = 2),
                        round(100*nrow(wiek_46_55_8_10)/suma_18_20, digits = 2), round(100*nrow(wiek_56_65_18_20)/suma_18_20, digits = 2), round(100*nrow(wiek_66_18_20)/suma_18_20, digits = 2))
  )
  godziny_wiek
}
library(ggplot2)
czas_wiek<-function(ramka){
  wiek_czas<-function(w1,w2){
    tab<-subset(ramka,birth.year<=2019-w1 & birth.year>=2019-w2 & is.numeric(birth.year), select=tripduration)
    wyn<-mean(tab[["tripduration"]])
    wyn
  }
  tabela_czas_wiek<-data.frame(
    wiek=c("16-25","26-35","36-45","46-55","56-65","66+"),
    œredni_czas_jazdy=c(wiek_czas(16,25)/60,wiek_czas(26,35)/60,wiek_czas(36,45)/60,wiek_czas(46,55)/60,wiek_czas(56,65)/60,wiek_czas(66,90)/60)
  )
  ggplot(tabela_czas_wiek, aes(wiek, œredni_czas_jazdy)) + geom_col(color="red", fill="yellow")
}
plec<-function(p){
  if (p==0){
    wyn<-"nieznana"
  }
  if (p==1){
    wyn<-"mê¿czyzna"
  }
  if (p==2){
    wyn<-"kobieta"
  }
  wyn
}
sredni_czas<-function(tab){
  df<-round(mean(tab[["tripduration"]]/60,digits=0))
  df
}
wiek_plec_czas<-function(ramka,w1,w2,p){
  tab<-subset(ramka,birth.year<=2019-w1 & birth.year>=2019-w2 & is.numeric(birth.year) & gender==p)
  if (nrow(tab)>0){
    stacje_koncowe<-aggregate(x=tab$end.station.name, by=tab["end.station.name"], FUN=length)
    ulubiona<-as.vector(stacje_koncowe[which.max(stacje_koncowe$x),][1,1])
    tabela_czas_wiek<-data.frame(wiek.min=c(w1),wiek.max=c(w2),p³eæ=c(plec(p)),liczba_przejazdów=c(nrow(tab)),
                                 œredni_czas_przejazdu=c(sredni_czas(tab)),ulubiona_stacja=c(ulubiona))
  }
  else{
    tabela_czas_wiek<-data.frame(wiek.min=c(w1),wiek.max=c(w2),p³eæ=c(plec(p)), liczba_przejazdów=c(nrow(tab)),
                                 œredni_czas_przejazdu=0,ulubiona_stacja=c("brak danych"))
  }
  tabela_czas_wiek
}
wiek_czas<-function(ramka,w1,w2){
  df<-rbind(wiek_plec_czas(ramka,w1,w2,0),wiek_plec_czas(ramka,w1,w2,1),wiek_plec_czas(ramka,w1,w2,2))
  df
}
czas_wiek_plec<-function(ramka){
  wyn<-rbind(wiek_czas(ramka,16,25),wiek_czas(ramka,26,35),wiek_czas(ramka,36,45),wiek_czas(ramka,46,55),wiek_czas(ramka,56,65),wiek_czas(ramka,65,90))
  wyn
  #nie u¿ywamy tej funkcji w prezentacji, ale zwraca ona ciekawe wyniki
}
dla_plci<-function(ramka,p){
  tab<-subset(ramka,gender==p)
  if(nrow(tab)>0){
    stacje_koncowe<-aggregate(x=tab$end.station.name, by=tab["end.station.name"], FUN=length)
    ulubiona<-as.vector(stacje_koncowe[which.max(stacje_koncowe$x),][1,1])
    tabela<-data.frame(p³eæ=c(plec(p)),przejazdy=c(nrow(tab)),œredni_czas=c(sredni_czas(tab)),stacja_koñcowa=c(ulubiona))
    tabela
  }
}
plcie<-function(ramka){
  wyn<-rbind(dla_plci(ramka,0),dla_plci(ramka,1),dla_plci(ramka,2))
  wyn
}
plcie_procentowo<-function(ramka){
  tab0<-subset(ramka,gender==0)
  tab1<-subset(ramka,gender==1)
  tab2<-subset(ramka,gender==2)
  tabela<-data.frame(p³eæ=c("nieznana","mê¿czyzna","kobieta"),
                     udzia³_w_przejazdach=c(round(100*nrow(tab0)/nrow(ramka),digits=2),round(100*nrow(tab1)/nrow(ramka),digits=2),round(100*nrow(tab2)/nrow(ramka),digits=2)),
                     udzia³_w_czasie=c(round(100*sum(tab0[["tripduration"]])/sum(ramka[["tripduration"]]),digits=2),round(100*sum(tab1[["tripduration"]])/sum(ramka[["tripduration"]]),digits=2),round(100*sum(tab2[["tripduration"]])/sum(ramka[["tripduration"]]),digits=2)))
  tabela
}
wiek_procentowo<-function(ramka){
  tab16<-subset(ramka,is.numeric(birth.year) & birth.year<=2003 & birth.year>=1994)
  tab26<-subset(ramka,is.numeric(birth.year) & birth.year<=1993 & birth.year>=1984)
  tab36<-subset(ramka,is.numeric(birth.year) & birth.year<=1983 & birth.year>=1974)
  tab46<-subset(ramka,is.numeric(birth.year) & birth.year<=1973 & birth.year>=1964)
  tab56<-subset(ramka,is.numeric(birth.year) & birth.year<=1963 & birth.year>=1954)
  tab66<-subset(ramka,is.numeric(birth.year) & birth.year<=1953 & birth.year>=1940)
  wiek<-c("16-25","26-35","36-45","46-55","56-65","66+")
  udzia³_w_przejazdach<-c(round(100*nrow(tab16)/nrow(ramka),digits=2),round(100*nrow(tab26)/nrow(ramka),digits=2),round(100*nrow(tab36)/nrow(ramka),digits=2),round(100*nrow(tab46)/nrow(ramka),digits=2),round(100*nrow(tab56)/nrow(ramka),digits=2),round(100*nrow(tab66)/nrow(ramka),digits=2))
  wiek<-paste(wiek, udzia³_w_przejazdach, sep = " - ")
  wiek<-paste(wiek, "%", sep = "")
  pie(udzia³_w_przejazdach, labels = wiek,col = rainbow(length(wiek)), main = "Udzia³ procentowy w przejazdach w zale¿noœci od wieku")
}
udzial_stacji<-function(ramka){
  udzialowe<-aggregate(x=ramka$end.station.name, by=ramka["end.station.name"], FUN=length)
  udzialowe$x<-round(100*udzialowe$x/nrow(ramka),digits=3)
  udzialowe <- udzialowe[order(udzialowe$x,decreasing = TRUE),]
  row.names(udzialowe)<-NULL
  colnames(udzialowe)<-c("stacja_koñcowa","procentowy_udzia³")
  udzialowe
}
swieto_vs_wakacje<-function(){
  lipiec2019$DataStartu <- sapply(strsplit(as.character(lipiec2019$starttime), " "), "[", 1)
  lipiec2019$CzasStartu <- sapply(strsplit(as.character(lipiec2019$starttime), " "), "[", 2)
  lipiec2019$DataKonca <- sapply(strsplit(as.character(lipiec2019$stoptime), " "), "[", 1)
  lipiec2019$CzasKonca <- sapply(strsplit(as.character(lipiec2019$stoptime), " "), "[", 2)
  lipiec2019_czwarty_lipca<-subset(lipiec2019, DataKonca=="2019-07-04")
  wyn<-merge(udzial_stacji(lipiec2019_czwarty_lipca),udzial_stacji(wakacje2019),by='stacja_koñcowa')
  colnames(wyn)<-c("stacja_koñcowa","udzia³_4.07","udzia³_wakacje")
  wyn<-subset(wyn, wyn[2]>3 & wyn[3]>3)
  row.names(wyn)<-NULL
  wyn
}
