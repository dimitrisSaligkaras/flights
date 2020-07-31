

#Ερώτηση 1:να βρείτε (αν υπάρχουν) και να εμφανίσετε το πλήθος των κενών γραμμών σε κάθε στήλη του dataset
str(delayedflights_csv)
a=data.frame(delayedflights_csv)
which(is.na(a))
#ara yparxoyn kena stoixeia
colSums(is.na(a))
a
#Ερώτηση 2:να υπολογίσετε και να εμφανίσετε ποια ημέρα σε ποιον μήνα σημειώθηκαν οι περισσότερες καθυστερήσεις πτήσεων


library(dplyr)
a %>%
  select(Month,DayofMonth,ArrDelay)%>%
  filter(ArrDelay>=0)%>%
  group_by(Month,DayofMonth) %>%
  tally()%>%
  arrange(desc(n))


#Ερώτηση 3: να υπολογίσετε και να εμφανίσετε τον ημερήσιο μέσο όρο καθυστερήσεων για καθέναν από τους θερινούς μήνες του 2008
a %>%
 filter(Month %in% c(6,7,8))%>%
select(Month,DayofMonth,ArrDelay)%>%
  group_by(Month)%>%
  summarise(v=n())%>%
  mutate(d=v/30)%>%
  arrange(desc(d))


#Ερώτηση 4: να υπολογίσετε και να εμφανίσετε το όνομα της αεροπορικής εταιρίας που είχε το μεγαλύτερο πλήθος κωδικών ακύρωσης τύπου Β
a %>%
 filter(CancellationCode=="B")%>%
  select(UniqueCarrier,CancellationCode)%>%
  group_by(UniqueCarrier)%>%
  summarise(order=n())%>%
  arrange(desc(order))

#Ερώτηση 5: να βρείτε τους κωδικούς των πτήσεων με τον μεγαλύτερο αριθμό καθυστερήσεων
a%>%
  filter(ArrDelay>=0)%>%
  select(FlightNum,ArrDelay)%>%
  group_by(FlightNum) %>%
  summarise(v=n())%>%
  arrange(desc(v))

#Ερώτηση 6: να βρείτε και να υπολογίσετε το όνομα του μεγαλύτερου σε απόσταση προορισμού με τις περισσότερες καθυστερήσεις
   a%>%
  select(Origin,Dest,Distance,ArrDelay)%>%
group_by(Origin,Dest)%>%
  summarise(k=sum(ArrDelay,na.rm=TRUE))%>%
  arrange(desc(k))
  
  
#Ερώτηση 7: να βρείτε και να εμφανίσετε τους προορισμούς που είχαν την μεγαλύτερη καθυστέρηση (πτήσεις που εκτελέστηκαν)
a %>%
 filter(CancellationCode=="N")%>%
  select(CancellationCode,Dest,ActualElapsedTime)%>%
  group_by(Dest)%>%
  summarise(v=n())
arrange(desc(v))

#Ερώτηση 8: να βρείτε και να εμφανίσετε το όνομα της αεροπορικής εταιρείας που είχε τις μεγαλύτερες καθυστερήσεις που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών
a%>%
  select(UniqueCarrier,LateAircraftDelay)%>%
  group_by(UniqueCarrier)%>%
  tally()%>%
arrange(desc(n))%>%
slice(1)
#Ερώτηση 9: να υπολογίσετε πόσες ακυρώσεις πτήσεων τύπου Α σημειώθηκαν την 13η ημέρα κάθε μήνα
a%>%
  filter(DayofMonth==13 & CancellationCode=="A")%>%
  select(Month,DayofMonth,CancellationCode)%>%
  group_by(Month)%>%
 tally()%>%
  arrange(desc(n))
#Ερώτηση 10: υπολογίσετε και να εμφανίσετε την μέση καθυστέρηση πτήσεων που εκτελέστηκαν από την 10η μέχρι την 23 Απριλίου 2008
a%>%
  filter(DayofMonth %in% c(10:23)& Month==4)%>%
  select(DayofMonth,Month,ArrDelay)%>%
  group_by(DayofMonth)%>%
  summarise(v=mean(ArrDelay))%>%
  arrange(desc(v))
#Ερώτηση 11: να υπολογίσετε και να εμφανίσετε τον μήνα που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε έλεγχους ασφαλείας κατά τις ώρες 06.00-14.00
 a%>%
filter(DepTime>=600 & DepTime<=1400)%>%
   select(DepTime,SecurityDelay,Month)%>%
   group_by(Month)%>%
   summarise(v=n())%>%
   arrange(desc(v))%>%
   slice(1)
 
 
#Ερώτηση 12: να υπολογίσετε και να εμφανίσετε ποιος κωδικός πτήσης(αριθμός πτήσης) είχε το πρώτο δεκαήμερο του Νοεμβρίου του 2008 την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό της
a%>%
 filter(DayofMonth %in% c(1:10)& Month==11& ArrTime<CRSArrTime)%>%
  select(FlightNum,DayofMonth,Month,ArrTime,CRSArrTime)%>%
  group_by(FlightNum)%>%
  summarise(v=sum(ArrTime-CRSArrTime))%>%
  arrange(v)%>%
  slice(1)


#Ερώτηση 13: να υπολογίσετε και να εμφανίσετε ποιο αεροδρόμιο (τοποθεσία αναχώρησης) είχε το δεύτερο δεκαήμερο του Αυγούστου 2018 τις περισσότερες πτήσεις με καθυστέρηση(αναχωρίσεων) μεγαλύτερη από μισή ώρα που οφείλονται στους αερομεταφορείς
  a%>%
  filter(DayofMonth %in% c(11:20)& Month==8 & DepDelay>30 & CarrierDelay>0)%>%
select(DayofMonth,Month,DepDelay,CarrierDelay,Origin)%>%
  group_by(Origin)%>%
  summarise(order=n())%>%
  arrange(desc(order))
 
#Ερώτηση 14: να βρείτε και να εμφανίσετε τις πτήσεις που εκτράπηκαν από την πορεία τους αλλά ολοκληρώθηκαν καθώς και τον συνολικό χρόνο που απαιτήθηκε
a%>%
  filter(Diverted==1&CancellationCode=="N")%>%
  select(Diverted,CancellationCode,CRSElapsedTime,DepDelay,FlightNum,TailNum)%>%
  group_by(FlightNum,TailNum)%>%
  summarise(v=sum(CRSElapsedTime+DepDelay))%>%
  arrange(desc(v))

#Ερώτηση 15: ποιος μήνας είχε την μεγαλύτερη τυπική απόκλιση σε καθυστερήσεις ("πιο απρόβλεπτος μήνας"). Ως απόκλιση να θεωρηθεί η διαφορά ανάμεσα στον προγραμματισμένο και τον πραγματικό χρόνο εκτέλεσης της πτήσης
 DelayedFlights$var=with(DelayedFlights,CRSElapsedTime-ActualElapsedTime) 
 str(DelayedFlights)
 a=as.data.frame(DelayedFlights)
a%>%
  filter(var<0)%>%
  select(Month)%>%
  group_by(Month)%>%
  tally()%>%
 arrange(desc(n))



