# spit out misisng data

colSums(is.na(data_triphase))


data_triphase[is.na(data_triphase$integral),]
data_triphase[is.na(data_triphase$total_time_incub),]

