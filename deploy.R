
library(rsconnect)

removeAccount("bruno-lima") # bioestatisticas

rsconnect::setAccountInfo(name='balima', 
                          token='7C70EA0A1281B4A1039E22690742872E', 
                          secret='uUpBl8mr3Dd83+wHNxcqCLK3It0+pFQkWkfabn4d')


rsconnect::deployApp("D:/PhD/HEADS/kars")


getwd()
