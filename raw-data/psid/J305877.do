#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 305877                            
   DATA_DOMAIN      : FAM                               
   USER_WHERE       : NULL                              
   FILE_TYPE        : NULL                              
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 218                               
   N_OF_OBSERVATIONS: 32088                             
   MAX_REC_LENGTH   : 827                               
   DATE & TIME      : April 15, 2022 @ 14:33:26
*************************************************************************
;

global path "~/Dropbox/USWealthDynamics/raw-data/psid";

infix
      V1                   1 - 1           V3                   2 - 5      long V81                  6 - 10    
      V439                11 - 12          V441                13 - 13          V442                14 - 17    
 long V529                18 - 22          V1014               23 - 24          V1101               25 - 25    
      V1102               26 - 29     long V1514               30 - 34          V1609               35 - 36    
      V1801               37 - 37          V1802               38 - 41     long V2226               42 - 46    
      V2321               47 - 48          V2401               49 - 49          V2402               50 - 53    
 long V2852               54 - 58          V2968               59 - 60          V3001               61 - 61    
      V3002               62 - 65     long V3256               66 - 70          V3301               71 - 72    
      V3401               73 - 73          V3402               74 - 77     long V3676               78 - 82    
      V3721               83 - 84          V3801               85 - 85          V3802               86 - 89    
 long V4154               90 - 94          V4224               95 - 96          V4301               97 - 97    
      V4302               98 - 101    long V5029              102 - 106         V5099              107 - 108   
      V5201              109 - 109         V5202              110 - 113    long V5626              114 - 118   
      V5665              119 - 120         V5701              121 - 121         V5702              122 - 125   
 long V6173              126 - 130         V6212              131 - 132         V6301              133 - 133   
      V6302              134 - 137    long V6766              138 - 142         V6805              143 - 144   
      V6901              145 - 145         V6902              146 - 149    long V7412              150 - 155   
      V7451              156 - 157         V7501              158 - 158         V7502              159 - 162   
 long V8065              163 - 169         V8103              170 - 171         V8201              172 - 172   
      V8202              173 - 176    long V8689              177 - 183         V8727              184 - 185   
      V8801              186 - 186         V8802              187 - 190    long V9375              191 - 197   
      V9421              198 - 198         V9433              199 - 200         V10001             201 - 201   
      V10002             202 - 205    long V11022             206 - 211         V11067             212 - 212   
      V11079             213 - 214    long S117               215 - 223         S117A              224 - 224   
      V11101             225 - 225         V11102             226 - 229    long V12371             230 - 235   
      V12428             236 - 236         V12446             237 - 238         V12501             239 - 239   
      V12502             240 - 243    long V13623             244 - 250         V13667             251 - 251   
      V13687             252 - 253         V13701             254 - 254         V13702             255 - 258   
 long V14670             259 - 265         V14714             266 - 266         V14737             267 - 268   
      V14801             269 - 269         V14802             270 - 273    long V16144             274 - 280   
      V16189             281 - 281         V16208             282 - 283         V16301             284 - 284   
      V16302             285 - 288    long V17533             289 - 295         V17567             296 - 296   
      V17612             297 - 300    long S217               301 - 309         S217A              310 - 310   
      V17701             311 - 311         V17702             312 - 316    long V18875             317 - 323   
      V18918             324 - 324         V18943             325 - 331         V19001             332 - 332   
      V19002             333 - 336    long V20175             337 - 343         V20218             344 - 344   
      V20243             345 - 351         V20301             352 - 352         V20302             353 - 356   
 long V21481             357 - 363         V21524             364 - 364         V21547             365 - 371   
      V21601             372 - 372         V21602             373 - 377    long V23322             378 - 384   
      V23338             385 - 385         V23361             386 - 392         ER2001             393 - 393   
      ER2002             394 - 398    long ER4153             399 - 405         ER4159C            406 - 406   
      ER4160             407 - 413    long S317               414 - 422         S317A              423 - 423   
      ER5001             424 - 424         ER5002             425 - 429    long ER6993             430 - 436   
      ER6999C            437 - 437         ER7000             438 - 444         ER7001             445 - 445   
      ER7002             446 - 449    long ER9244             450 - 456         ER9250C            457 - 457   
      ER9251             458 - 464         ER10001            465 - 465         ER10002            466 - 470   
 long ER12079            471 - 479         ER12084            480 - 486         ER12223C           487 - 487   
      ER13001            488 - 488         ER13002            489 - 493         ER16425            494 - 494   
 long ER16462            495 - 501         ER16518            502 - 508    long S417               509 - 517   
      S417A              518 - 518         ER17001            519 - 519         ER17002            520 - 523   
      ER20371            524 - 524         ER20394            525 - 531    long ER20456            532 - 538   
 long S517               539 - 547         S517A              548 - 548         ER21001            549 - 549   
      ER21002            550 - 554    long ER24099            555 - 561         ER24152            562 - 562   
      ER24179            563 - 569    long S617               570 - 578         S617A              579 - 579   
      ER25001            580 - 580         ER25002            581 - 585    long ER28037            586 - 592   
      ER28051            593 - 593         ER28078            594 - 600    long S717               601 - 609   
      S717A              610 - 610         ER36001            611 - 611         ER36002            612 - 616   
 long ER41027            617 - 623         ER41041            624 - 624         ER41069            625 - 631   
 long S817               632 - 640         S817A              641 - 641         ER42001            642 - 642   
      ER42002            643 - 647    long ER46935            648 - 654    long ER46970            655 - 663   
      ER46971            664 - 664         ER46985            665 - 665         ER47012            666 - 672   
      ER47301            673 - 673         ER47302            674 - 678    long ER52343            679 - 685   
 long ER52394            686 - 694         ER52395            695 - 695         ER52409            696 - 696   
      ER52436            697 - 703         ER53001            704 - 704         ER53002            705 - 709   
 long ER58152            710 - 716    long ER58211            717 - 725         ER58212            726 - 726   
      ER58227            727 - 727         ER58257            728 - 734         ER60001            735 - 735   
      ER60002            736 - 740    long ER65349            741 - 747    long ER65408            748 - 756   
      ER65409            757 - 757         ER65463            758 - 758         ER65492            759 - 765   
      ER66001            766 - 766         ER66002            767 - 771    long ER71426            772 - 778   
 long ER71485            779 - 787         ER71486            788 - 788         ER71542            789 - 789   
      ER71570            790 - 796         ER72001            797 - 797         ER72002            798 - 802   
 long ER77448            803 - 809    long ER77511            810 - 818         ER77512            819 - 819   
      ER77603            820 - 820         ER77631            821 - 827   
using ${path}/J305877.txt, clear 
;
label variable V1              "RELEASE NUMBER"                           ;
label variable V3              "FAMILY NUMBER"                            ;
label variable V81             "FAM MONEY INC"                            ;
label variable V439            "1968 WEIGHT"                              ;
label variable V441            "RELEASE NUMBER"                           ;
label variable V442            "1969 INT NUMBER"                          ;
label variable V529            "TOTAL FU $  INC"                          ;
label variable V1014           "1969 FAMILY WEIGHT"                       ;
label variable V1101           "RELEASE NUMBER"                           ;
label variable V1102           "1970 INT #"                               ;
label variable V1514           "TOT FU MON INC OV414"                     ;
label variable V1609           "WEIGHT"                                   ;
label variable V1801           "RELEASE NUMBER"                           ;
label variable V1802           "71 ID NO."                                ;
label variable V2226           "TOT FU MON INC"                           ;
label variable V2321           "WEIGHT"                                   ;
label variable V2401           "RELEASE NUMBER"                           ;
label variable V2402           "1972 INT #"                               ;
label variable V2852           "TOT FU MON INC"                           ;
label variable V2968           "1972 WEIGHTS"                             ;
label variable V3001           "RELEASE NUMBER"                           ;
label variable V3002           "1973 INT #"                               ;
label variable V3256           "TOT FU MON INC"                           ;
label variable V3301           "1973 FAMILY WEIGHT"                       ;
label variable V3401           "RELEASE NUMBER"                           ;
label variable V3402           "1974 ID NUMBER"                           ;
label variable V3676           "TOT FU MON INC"                           ;
label variable V3721           "1974 FAMILY WEIGHT"                       ;
label variable V3801           "RELEASE NUMBER"                           ;
label variable V3802           "1975 INT #"                               ;
label variable V4154           "TOT FU MON INC"                           ;
label variable V4224           "1975 FAMILY WEIGHT"                       ;
label variable V4301           "RELEASE NUMBER"                           ;
label variable V4302           "1976 ID NUMBER"                           ;
label variable V5029           "TOT FU MONEY INC"                         ;
label variable V5099           "1976 FAMILY WEIGHT"                       ;
label variable V5201           "RELEASE NUMBER"                           ;
label variable V5202           "1977 ID"                                  ;
label variable V5626           "TOT 1976 FAM MONEY INCOM"                 ;
label variable V5665           "1977 FAMILY WEIGHT"                       ;
label variable V5701           "RELEASE NUMBER"                           ;
label variable V5702           "1978 ID"                                  ;
label variable V6173           "TOT 1977 FAM $  INC"                      ;
label variable V6212           "78 UPDATED FAM WEIGHT"                    ;
label variable V6301           "RELEASE NUMBER"                           ;
label variable V6302           "1979 ID"                                  ;
label variable V6766           "TOT 1978 FAM $  INC"                      ;
label variable V6805           "79 FAMILY WEIGHT"                         ;
label variable V6901           "RELEASE NUMBER"                           ;
label variable V6902           "1980 INTERVIEW NUMBER"                    ;
label variable V7412           "TOT FAM $ Y 79"                           ;
label variable V7451           "1980 FAMILY WEIGHT"                       ;
label variable V7501           "RELEASE NUMBER"                           ;
label variable V7502           "1981 INTERVIEW NUMBER"                    ;
label variable V8065           "TOT FAM $ $  Y 80"                        ;
label variable V8103           "1981 FAMILY WEIGHT"                       ;
label variable V8201           "RELEASE NUMBER"                           ;
label variable V8202           "1982 INTERVIEW NUMBER"                    ;
label variable V8689           "TOT FAM $ $  Y 81"                        ;
label variable V8727           "1982 FAMILY WEIGHT"                       ;
label variable V8801           "RELEASE NUMBER"                           ;
label variable V8802           "1983 INTERVIEW NUMBER"                    ;
label variable V9375           "TOT FAM MONEY Y 82"                       ;
label variable V9421           "COUPLE STATUS OF HEAD"                    ;
label variable V9433           "1983 FAMILY WEIGHT"                       ;
label variable V10001          "RELEASE NUMBER"                           ;
label variable V10002          "1984 INTERVIEW NUMBER"                    ;
label variable V11022          "TOT FAM MONEY Y 83"                       ;
label variable V11067          "COUPLE STATUS OF HEAD"                    ;
label variable V11079          "1984 FAMILY WEIGHT"                       ;
label variable S117            "IMP WEALTH W/ EQUITY (WEALTH2) 84"        ;
label variable S117A           "ACC WEALTH W/ EQUITY (WEALTH2) 84"        ;
label variable V11101          "RELEASE NUMBER"                           ;
label variable V11102          "1985 INTERVIEW NUMBER"                    ;
label variable V12371          "TOT FAM MONEY Y 84"                       ;
label variable V12428          "COUPLE STATUS OF HEAD"                    ;
label variable V12446          "1985 FAMILY WEIGHT"                       ;
label variable V12501          "RELEASE NUMBER"                           ;
label variable V12502          "1986 INTERVIEW NUMBER"                    ;
label variable V13623          "TOT FAM MONEY Y 85"                       ;
label variable V13667          "COUPLE STATUS OF HEAD"                    ;
label variable V13687          "1986 FAMILY WEIGHT"                       ;
label variable V13701          "RELEASE NUMBER"                           ;
label variable V13702          "1987 INTERVIEW NUMBER"                    ;
label variable V14670          "TOT FAM MONEY Y 86"                       ;
label variable V14714          "COUPLE STATUS OF HEAD"                    ;
label variable V14737          "1987 FAMILY WEIGHT"                       ;
label variable V14801          "RELEASE NUMBER"                           ;
label variable V14802          "1988 INTERVIEW NUMBER"                    ;
label variable V16144          "TOT FAM MONEY Y 87"                       ;
label variable V16189          "COUPLE STATUS OF HEAD"                    ;
label variable V16208          "1988 FAMILY WEIGHT"                       ;
label variable V16301          "RELEASE NUMBER"                           ;
label variable V16302          "1989 INTERVIEW NUMBER"                    ;
label variable V17533          "TOT FAM MONEY Y 88"                       ;
label variable V17567          "COUPLE STATUS OF HEAD"                    ;
label variable V17612          "1989 FAMILY WEIGHT"                       ;
label variable S217            "IMP WEALTH W/ EQUITY (WEALTH2) 89"        ;
label variable S217A           "ACC WEALTH W/ EQUITY (WEALTH2) 89"        ;
label variable V17701          "RELEASE NUMBER"                           ;
label variable V17702          "1990 INTERVEW NUMBER"                     ;
label variable V18875          "TOT FAM MONEY Y 89"                       ;
label variable V18918          "COUPLE STATUS OF HEAD"                    ;
label variable V18943          "1990 CORE FAMILY WEIGHT"                  ;
label variable V19001          "RELEASE NUMBER"                           ;
label variable V19002          "1991 INTERVIEW NUMBER"                    ;
label variable V20175          "TOT FAM MONEY Y 90"                       ;
label variable V20218          "COUPLE STATUS OF HEAD"                    ;
label variable V20243          "1991 CORE FAMILY WEIGHT"                  ;
label variable V20301          "RELEASE NUMBER"                           ;
label variable V20302          "1992 INTERVIEW NUMBER"                    ;
label variable V21481          "TOT FAM MONEY Y 91"                       ;
label variable V21524          "COUPLE STATUS OF HEAD"                    ;
label variable V21547          "1992 CORE FAMILY WEIGHT"                  ;
label variable V21601          "RELEASE NUMBER"                           ;
label variable V21602          "1993 INTERVIEW NUMBER"                    ;
label variable V23322          "TOTAL 1992 FAMILY MONEY INCOME"           ;
label variable V23338          "COUPLE STATUS OF HEAD"                    ;
label variable V23361          "1993 LONGITUDINAL CORE FAMILY WEIGHT"     ;
label variable ER2001          "RELEASE NUMBER"                           ;
label variable ER2002          "1994 INTERVIEW #"                         ;
label variable ER4153          "TOTAL FAMILY INCOME-1993"                 ;
label variable ER4159C         "COUPLE STATUS OF HEAD"                    ;
label variable ER4160          "1994 FAMILY WEIGHT"                       ;
label variable S317            "IMP WEALTH W/ EQUITY (WEALTH2) 94"        ;
label variable S317A           "ACC WEALTH W/ EQUITY (WEALTH2) 94"        ;
label variable ER5001          "RELEASE NUMBER"                           ;
label variable ER5002          "1995 INTERVIEW #"                         ;
label variable ER6993          "TOTAL FAMILY INCOME-1994"                 ;
label variable ER6999C         "COUPLE STATUS OF HEAD"                    ;
label variable ER7000          "1995 LONGITUDINAL CORE FAMILY WEIGHT"     ;
label variable ER7001          "RELEASE NUMBER"                           ;
label variable ER7002          "1996 INTERVIEW #"                         ;
label variable ER9244          "TOTAL FAMILY INCOME-1995"                 ;
label variable ER9250C         "COUPLE STATUS OF HEAD"                    ;
label variable ER9251          "1996 FAMILY WEIGHT"                       ;
label variable ER10001         "RELEASE NUMBER"                           ;
label variable ER10002         "1997 INTERVIEW #"                         ;
label variable ER12079         "TOTAL FAMILY INCOME"                      ;
label variable ER12084         "FAMILY WEIGHT"                            ;
label variable ER12223C        "COUPLE STATUS OF HEAD"                    ;
label variable ER13001         "RELEASE NUMBER"                           ;
label variable ER13002         "1999 FAMILY INTERVIEW (ID) NUMBER"        ;
label variable ER16425         "COUPLE STATUS OF HEAD"                    ;
label variable ER16462         "TOTAL FAMILY INCOME"                      ;
label variable ER16518         "1999 CORE/IMMIGRANT FAMILY WEIGHT"        ;
label variable S417            "IMP WEALTH W/ EQUITY (WEALTH2) 99"        ;
label variable S417A           "ACC WEALTH W/ EQUITY (WEALTH2) 99"        ;
label variable ER17001         "RELEASE NUMBER"                           ;
label variable ER17002         "2001 FAMILY INTERVIEW (ID) NUMBER"        ;
label variable ER20371         "COUPLE STATUS OF HEAD"                    ;
label variable ER20394         "CORE/IMMIGRANT FAMILY WEIGHT NUMBER 1"    ;
label variable ER20456         "TOTAL FAMILY INCOME-2000"                 ;
label variable S517            "IMP WEALTH W/ EQUITY (WEALTH2) 01"        ;
label variable S517A           "ACC WEALTH W/ EQUITY (WEALTH2) 01"        ;
label variable ER21001         "RELEASE NUMBER"                           ;
label variable ER21002         "2003 FAMILY INTERVIEW (ID) NUMBER"        ;
label variable ER24099         "TOTAL FAMILY INCOME LAST YEAR"            ;
label variable ER24152         "COUPLE STATUS OF HEAD"                    ;
label variable ER24179         "CORE/IMM FAMILY LONGITUDINAL WEIGHT"      ;
label variable S617            "IMP WEALTH W/ EQUITY (WEALTH2) 03"        ;
label variable S617A           "ACC WEALTH W/ EQUITY (WEALTH2) 03"        ;
label variable ER25001         "RELEASE NUMBER"                           ;
label variable ER25002         "2005 FAMILY INTERVIEW (ID) NUMBER"        ;
label variable ER28037         "TOTAL FAMILY INCOME-2004"                 ;
label variable ER28051         "COUPLE STATUS OF HEAD"                    ;
label variable ER28078         "2005 CORE/IMMIGRANT FAM WEIGHT NUMBER 1"  ;
label variable S717            "IMP WEALTH W/ EQUITY (WEALTH2) 05"        ;
label variable S717A           "ACC WEALTH W/ EQUITY (WEALTH2) 05"        ;
label variable ER36001         "RELEASE NUMBER"                           ;
label variable ER36002         "2007 FAMILY INTERVIEW (ID) NUMBER"        ;
label variable ER41027         "TOTAL FAMILY INCOME-2006"                 ;
label variable ER41041         "COUPLE STATUS OF HEAD"                    ;
label variable ER41069         "2007 CORE/IMMIGRANT FAM WEIGHT NUMBER 1"  ;
label variable S817            "IMP WEALTH W/ EQUITY (WEALTH2) 07"        ;
label variable S817A           "ACC WEALTH W/ EQUITY (WEALTH2) 07"        ;
label variable ER42001         "RELEASE NUMBER"                           ;
label variable ER42002         "2009 FAMILY INTERVIEW (ID) NUMBER"        ;
label variable ER46935         "TOTAL FAMILY INCOME-2008"                 ;
label variable ER46970         "IMP WEALTH W/ EQUITY (WEALTH2) 09"        ;
label variable ER46971         "ACC WEALTH W/ EQUITY (WEALTH2) 09"        ;
label variable ER46985         "COUPLE STATUS OF HEAD"                    ;
label variable ER47012         "2009 CORE/IMMIGRANT FAM WEIGHT NUMBER 1"  ;
label variable ER47301         "RELEASE NUMBER"                           ;
label variable ER47302         "2011 FAMILY INTERVIEW (ID) NUMBER"        ;
label variable ER52343         "TOTAL FAMILY INCOME-2010"                 ;
label variable ER52394         "IMP WEALTH W/ EQUITY (WEALTH2) 11"        ;
label variable ER52395         "ACC WEALTH W/ EQUITY (WEALTH2) 11"        ;
label variable ER52409         "COUPLE STATUS OF HEAD"                    ;
label variable ER52436         "2011 CORE/IMMIGRANT FAM WEIGHT NUMBER 1"  ;
label variable ER53001         "RELEASE NUMBER"                           ;
label variable ER53002         "2013 FAMILY INTERVIEW (ID) NUMBER"        ;
label variable ER58152         "TOTAL FAMILY INCOME-2012"                 ;
label variable ER58211         "IMP WEALTH W/ EQUITY (WEALTH2) 2013"      ;
label variable ER58212         "ACC WEALTH W/ EQUITY (WEALTH2) 2013"      ;
label variable ER58227         "COUPLE STATUS OF HEAD"                    ;
label variable ER58257         "2013 CORE/IMMIGRANT FAM WEIGHT NUMBER 1"  ;
label variable ER60001         "RELEASE NUMBER"                           ;
label variable ER60002         "2015 FAMILY INTERVIEW (ID) NUMBER"        ;
label variable ER65349         "TOTAL FAMILY INCOME-2014"                 ;
label variable ER65408         "IMP WEALTH W/ EQUITY (WEALTH2) 2015"      ;
label variable ER65409         "ACC WEALTH W/ EQUITY (WEALTH2) 2015"      ;
label variable ER65463         "COUPLE STATUS OF HEAD"                    ;
label variable ER65492         "2015 CORE/IMMIGRANT FAM WEIGHT NUMBER 1"  ;
label variable ER66001         "RELEASE NUMBER"                           ;
label variable ER66002         "2017 FAMILY INTERVIEW (ID) NUMBER"        ;
label variable ER71426         "TOTAL FAMILY INCOME-2016"                 ;
label variable ER71485         "IMP WEALTH W/ EQUITY (WEALTH2) 2017"      ;
label variable ER71486         "ACC WEALTH W/ EQUITY (WEALTH2) 2017"      ;
label variable ER71542         "COUPLE STATUS OF REF PERSON"              ;
label variable ER71570         "2017 CORE/IMMIGRANT FAM WEIGHT NUMBER 1"  ;
label variable ER72001         "RELEASE NUMBER"                           ;
label variable ER72002         "2019 FAMILY INTERVIEW (ID) NUMBER"        ;
label variable ER77448         "TOTAL FAMILY INCOME-2018"                 ;
label variable ER77511         "IMP WEALTH W/ EQUITY (WEALTH2) 2019"      ;
label variable ER77512         "ACC WEALTH W/ EQUITY (WEALTH2) 2019"      ;
label variable ER77603         "COUPLE STATUS OF REF PERSON"              ;
label variable ER77631         "2019 CORE/IMMIGRANT FAM WEIGHT NUMBER 1"  ;
