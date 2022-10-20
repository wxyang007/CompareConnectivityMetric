library(sf)
library(tidyverse)
#====metric vector 4: equivalent connected area [conefor]=====
wd <- 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA'
setwd(wd)

shp <- read_sf('ca_pa_file_fixed.shp')
# on desktop
# shp <- read_sf('C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/ca_pa_file_fixed.shp')
# summary(shp$OBJECTID)

ca_bound <- read_sf('ca-state-boundary/CA_State_TIGER2016.shp')

non_ca_pa <- read_sf('C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/non_ca_pa.shp')
non_ca_pa$ifTarget = 0

# filter out only the pas in CA, these are the patches to iterate with
ca_pa <- shp[which((shp$ifPA == 1) & (shp$STATEFP == '06')), ]

coneforfolder = 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/conefor_results_2'
n_iter = 100

root_folder <- 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/results'
setwd(root_folder)
iters_result_path <- paste0(root_folder, '/100iterations_Oct16.csv')
iters_result <- read.csv(iters_result_path, sep = ",")
iters_result$area_buff_1 <- 0

for (n in 1:n_iter){
  print(n)

  #OBJECTIDs_to_keep = as.list(as.numeric(strsplit(iters_result[n,]$objectids_to_keep, ",")[[1]]))
  #filtered_pa <- ca_pa[ca_pa$OBJECTID %in% OBJECTIDs_to_keep, ]
  filtered_pa <- ca_pa
  filtered_pa$ifTarget = 1
  
  # PAs to keep and non-PAs and transboundary PAs
  keptpa_and_nonpa <- rbind(filtered_pa, non_ca_pa)
  
  # PAs to keep and transboundary PAs
  all_only_pa <- keptpa_and_nonpa[keptpa_and_nonpa$ifPA == 1, ]
  
  filtered_pa$int_area <- 0
  
  buffered <- st_buffer(filtered_pa, 10000)
  st_agr(buffered) = "constant"
  st_agr(all_only_pa) = "constant"
  intersect <- st_intersection(buffered, all_only_pa)

  intersect$area <- st_area(intersect)
  intersect$ID.new <- 1:nrow(intersect)

  buffered$contains <- st_contains(buffered, intersect)
  
  buffered$int_area <- 0
  
  for (x in 1:nrow(buffered)) {
    a <- buffered[x, ]$contains[1]
    b <- intersect[intersect$ID.new %in% a[[1]],]
    buffered[x,]$int_area <- as.numeric(sum(b$area))
  }
  
  intersected_area_buff <- buffered$int_area
  print(mean(intersected_area_buff))
  
  iters_result[n, ]$area_buff_1 = mean(intersected_area_buff)
  
}

write.table(iters_result, paste0(root_folder, '/100iterations_Oct16_rev.csv'), sep = ',', row.names = FALSE)


# 1
library(sf)
wd <- 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA'
setwd(wd)
coneforfolder = 'C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/conefor_results_2'

shp <- read_sf('ca_pa_file_fixed.shp')
# on desktop
# shp <- read_sf('C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/ca_pa_file_fixed.shp')
# summary(shp$OBJECTID)

ca_bound <- read_sf('ca-state-boundary/CA_State_TIGER2016.shp')


# filter out only the pas in CA, these are the patches to iterate with
ca_pa <- shp[which((shp$ifPA == 1) & (shp$STATEFP == '06')), ]

OBJECTIDs_to_keep = c(140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,
                     156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,
                     171,172,173,174,175,176,177,178,179,180,183,184,185,186,188,
                     189,190,191,193,194,195,196,197,198,200,201,202,203,204,205,
                     206,208,209,211,212,213,214,216,217,218,219,220,221,222,224,
                     225,226,227,230,231,232,233,234,235,237,238,239,240,242,244,
                     245,246,247,248,249,250,251,252,253,254,255,256,258,259,260,
                     261,262,263,264,265,267,269,270,271,272,273,274,275,276,277,
                     278,279,280,281,282,283,284,285,286,287,288,289,290,291,293,
                     294,295,296,298,299,301,302,303,304,305,306,307,308,309,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,366,367,368,369,370,371,372,373,374,376,377,378,379,380,381,382,383,384,385,386,387,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,416,417,418,419,420,422,423,424,425,426,427,429,430,431,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,495,496,497,498,499,500,501,503,504,505,507,508,509,511,512,513,514,516,517,518,519,520,521,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,545,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,562,563,564,565,567,568,569,570,571,572,573,574,575,576,577,578,580,582,583,584,585,586,587,588,589,590,591,592,593,594,596,599,600,602,603,604,605,606,607,608,609,610,611,612,613,614,615,616,617,618,619,620,621,622,623,625,626,628,629,630,631,632,633,634,635,636,637,638,639,640,641,642,643,644,645,646,647,648,649,650,651,652,653,654,655,656,657,658,659,660,662,663,664,665,666,667,668,669,670,671,672,673,674,675,676,677,678,679,680,681,682,683,684,685,686,687,688,691,692,693,694,695,696,697,698,699,700,702,703,704,705,706,707,708,709,710,711,712,713,714,715,716,717,718,719,720,722,723,725,727,728,729,730,731,732,733,735,736,737,738,740,741,742,743,744,745,746,747,748,749,750,751,752,753,754,755,756,757,758,759,760,761,762,763,764,766,767,768,769,770,772,773,774,775,776,777,778,779,780,782,783,784,785,786,787,788,789,790,791,792,793,794,795,796,797,798,799,800,801,802,804,805,806,807,808,809,810,812,813,814,815,816,817,819,820,821,822,823,824,825,827,828,829,830,831,832,833,834,835,836,837,839,840,841,842,843,844,845,846,847,849,850,851,853,854,856,857,859,860,861,862,863,864,865,866,867,868,869,870,871,872,873,874,875,876,877,878,879,880,881,882,883,884,885,886,887,888,889,890,891,892,894,895,896,897,898,899,900,901,902,903,904,905,906,907,909,910,911,912,913,914,915,916,917,918,920,921,922,924,925,926,927,929,930,931,932,933,934,935,936,937,938,939,941,942,943,944,945,946,947,948,949,951,952,953,954,955,956,957,958,959,960,961,962,963,964,965,966,968,969,970,972,973,975,976,977,978,979,980,981,982,984,986,987,988,989,990,991,992,993,994,995,996,997,998,999,1000,1002,1003,1004,1005,1006,1007,1008,1009,1010,1011,1012,1013,1014,1015,1016,1017,1019,1020,1021,1023,1024,1025,1026,1027,1028,1029,1030,1031,1032,1034,1035,1036,1038,1040,1041,1043,1044,1045,1047,1048,1049,1050,1051,1052,1054,1055,1056,1058,1059,1060,1061,1062,1063,1064,1065,1068,1069,1070,1071,1072,1073,1074,1075,1076,1077,1080,1081,1082,1083,1084,1085,1086,1087,1088,1089,1090,1091,1092,1094,1095,1096,1097,1098,1099,1101,1103,1104,1105,1106,1107,1108,1109,1110,1111,1112,1113,1114,1115,1116,1117,1118,1119,1120,1121,1123,1125,1126,1127,1128,1129,1130,1132,1133,1134,1135,1136,1137,1138,1139,1140,1141,1142,1143,1144,1145,1146,1147,1148,1149,1150,1151,1152,1153,1154,1155,1156,1157,1158,1159,1160,1161,1162,1163,1164,1165,1166,1167,1168,1169,1170,1171,1172,1173,1174,1175,1176,1177,1178,1179,1180,1181,1182,1183,1184,1185,1186,1187,1188,1189,1190,1191,1192,1194,1196,1198,1199,1200,1201,1202,1203,1205,1206,1208,1209,1210,1211,1212,1213,1214,1215,1216,1218,1219,1220,1221,1222,1224,1225,1226,1227,1228,1229,1230,1231,1232,1233,1234,1235,1236,1237,1238,1239,1240,1241,1242,1243,1244,1245,1246,1247,1248,1249,1250,1251,1252,1253,1254,1255,1256,1257,1258,1259,1260,1261,1262,1264,1265,1266,1267,1268,1270,1271,1272,1273,1274,1275,1276,1277,1278,1280,1281,1282,1284,1285,1286,1287,1288,1289,1290)
filtered_pa <- ca_pa[ca_pa$OBJECTID %in% OBJECTIDs_to_keep, ]
filtered_pa$ifTarget = 1

non_ca_pa <- read_sf('C:/Users/wyang80/Desktop/CONN_MEASURE/data/CA/non_ca_pa.shp')
non_ca_pa$ifTarget = 0

keptpa_and_nonpa <- rbind(filtered_pa, non_ca_pa)

# PAs to keep and transboundary PAs
all_only_pa <- keptpa_and_nonpa[keptpa_and_nonpa$ifPA == 1, ]

nodedf <- all_only_pa[c("OBJECTID", "AREA_GEO", "ifPA", "STATEFP")]
nodedf <- nodedf[nodedf$ifPA == 1, ]
nodedf$AREA_GEO[which(nodedf$STATEFP != '06')] <- 0
nodedf <- filtered_pa[c("OBJECTID", "AREA_GEO")]
nodedf$geometry <- NULL
thisiter <- paste0(coneforfolder, "\\", 1)
conefor_exe <- paste0(coneforfolder, '/coneforWin64.exe')

setwd(thisiter)
nodefile <- paste0(n, '_node.txt')
confile <- paste0(n, '_distance.txt')
p1 <- paste(conefor_exe, "-nodeFile", nodefile, "-conFile", confile, 
            "-t dist -confProb 10000 0.5 -PC -F -AWF -confAdj 10000 -IIC -BC onlyoverall")

shell(p1, intern = TRUE)
