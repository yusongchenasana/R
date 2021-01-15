library(XLConnect)
library(data.table)
library(ggplot2)

dir <- r"(C:\Users\YusongChen\Asana BioSciences, LLC\Asana Data Warehouse - Documents\Development\ASN002\ASN002AD-202\RDATA)"

pk <- paste0(dir, "\\ASN002AD-202_Syneos_PK_2020JUN08.xlsx")
pk <- data.table(readWorksheetFromFile(pk, sheet="180454AUNP Part A"))
pk[, `:=`(conc = as.numeric(RESULT),
          week = as.numeric(WEEK), 
          hour = as.numeric(HOUR))]
pk[, `:=`(conc = ifelse(is.na(conc), 0, conc), 
          hour = ifelse(is.na(hour), 0, hour),
          week = ifelse(is.na(week), 15, week))][, 
           wh := paste(formatC(week, width=2), " / ", hour)]

pk[, whd := week + sqrt(week)*(hour/16 + 6*(as.numeric(DOSE..mg.) - 40)/800/4)]
pk[, `:=`(ma = max(whd, na.rm=T),
          mi = min(whd,na.rm=T)), .(WEEK)]
pk[, whd := whd - (ma-mi)/2.5, .(WEEK)]
pk[, wh  := mean(whd, na.rm=T), .(week, hour)]
#pk[, lbl:=ifelse(week %in% c(1, 4), "0   2       6", "0   2")]
setnames(pk, "DOSE..mg.", "DOSE")
pk[, DOSE:= paste0(DOSE, "mg")]
pk <- pk[DOSE != "Placebomg"]
pk[WEEK=="ET", hour:=NA]
pk[, lconc := log(conc)]
pk[, lconc := ifelse(lconc == - Inf, NA, lconc)]

pk_sum <- pk[, .(n = .N, mean=mean(conc), 
          median = median(conc), 
          sd = sd(conc),
          min = min(conc),
          max=max(conc), 
          gmean=exp(mean(lconc, na.rm = T)),
          cv = sqrt(exp(var(lconc, na.rm=T))-1)), .(DOSE, week, hour)]




ggplot(pk, 
       aes(x = whd, y = conc, group = as.factor(whd), 
          color=as.factor(DOSE))) + 
  geom_boxplot() + 
  scale_x_sqrt(breaks=c(1, 2, 4, 8, 12, 15), labels=c("1", "2", "4", "8", "12", "ET")) +
  scale_y_continuous(breaks=seq(0, 600, by= 100)) +
  theme_bw() + 
  geom_text(aes(x = as.numeric(wh), y = -15, label=hour), color = "gray30", size=3) +
  geom_text(aes(x = 5.7, y = -30, label="Hour"), colour = "gray30", size=4) +
  
  guides(color = guide_legend("ASN002")) +
  labs(x="Week", y="Plasma ASN002 Concentration (ng/mL)",
       title="Boxplot of Plasma ASN002 Concentration vs Time\n ASN002AD-202")





