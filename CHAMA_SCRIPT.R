#### Leitura banco de dados aferidos CHAMA

library(readr)
Afericao <- read_csv("~/Rstudio/Variabilidade Pressão Arterial.csv")
                      
#PAS Média 23

PASM23 <- rowMeans(cbind(Afericao$PAS2,Afericao$PAS3), na.rm=TRUE)

PASM23 <- round(PASM23)

#PAS Média 56

PASM56 <- rowMeans(cbind(Afericao$PAS5,Afericao$PAS6), na.rm=TRUE)


PASM56 <- round(PASM56)

#PAD Média 23

PADM23 <- rowMeans(cbind(Afericao$PAD2,Afericao$PAD3), na.rm=TRUE)

PADM23 <- round(PADM23)

#PAD 56

PADM56 <- rowMeans(cbind(Afericao$PAD5,Afericao$PAD6), na.rm=TRUE)

PADM56 <- round(PADM56)

#PAM 23

PAM23 <- (PASM23 + 2*PADM23)/3

PAM23 <- round(PAM23)

#PAM 56

PAM56 <- (PASM56 + 2*PADM56)/3

PAM56 <- round(PAM56)

#PP 23

PP23 <- PASM23-PADM23

#PP 56

PP56<- PASM56-PADM56


#FC23

FC23 <- rowMeans(cbind(Afericao$FC2,Afericao$FC3), na.rm=TRUE)

#FC 56

FC56 <- rowMeans(cbind(Afericao$FC5,Afericao$FC6), na.rm=TRUE)

#Juntanto os dados obtidos:

CHAMA <- cbind(PASM23, PASM56, PADM23, PADM56, PAM23, PAM56, PP23, PP56)

CHAMA <- as.data.frame(CHAMA)

#Qui-quadrado: 

#### Qui-qudrado

R1 <- table(CHAMA$PASM23, CHAMA$PASM56)

R2 <- table(CHAMA$PADM23, CHAMA$PADM56)

R3 <- table(CHAMA$PAM23, CHAMA$PAM56)

R4 <- table(CHAMA$PP23, CHAMA$PP56)


C1 <- chisq.test(R1, correct = TRUE)

C2 <- chisq.test(R2, correct = TRUE)

C3 <- chisq.test(R3, correct = TRUE)

C4 <- chisq.test(R4, correct = TRUE)





t.test(PADM123, PADM456, alt="two.sided", conf.level = 0.95)

t.test(PASM123, PASM456, alt="two.sided", conf.level = 0.95)

t.test(PAM123, PAM456, alt="two.sided", conf.level = 0.95)

t.test(PP123, PP456, alt="two.sided", conf.level = 0.95)

t.test(PADM23, PADM56, alt="two.sided", conf.level = 0.95)

t.test(PAM23, PAM56, alt="two.sided", conf.level = 0.95)

t.test(PASM23, PASM56, alt="two.sided", conf.level = 0.95)

t.test(PP23, PP56, alt="two.sided", conf.level = 0.95)






#Exportando

library(xlsx)
write.xlsx(CHAMA, "C:/Users/Rafael/Documents/Rstudio/CHAMA.xlsx")
