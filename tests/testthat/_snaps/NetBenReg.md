# NetBenReg snapshot

    Code
      NetBenReg(Followup = CEdata$survival, delta = CEdata$dead, group = CEdata$Trt,
      Cost = CEdata[, 8:22], Eff = CEdata[, 24:38], Part.times = 1:15, Method = "SW",
      Z = CEdata[, 5:7], Eff.only = TRUE, Cost.only = TRUE, lambda = lambda, L = 10)
    Output
      All n = 200 , Used n = 200 
      Time limit horizon L = 10 
      Censoring rate within L = 44.5 %
      Method: Simple Weighted
      
      lambda = 3:
                    Estimate  Std.err        Wald            p
      (Intercept) -7.4412657 1.400897 28.21508743 1.085551e-07
      group        0.4616494 1.771762  0.06789124 7.944322e-01
      Age651      -2.4769629 1.986734  1.55438839 2.124888e-01
      LBBB1        5.1553997 1.860940  7.67467170 5.600128e-03
      Female1     -2.3549930 1.771840  1.76656683 1.838075e-01
      
      for Effect:
                    Estimate   Std.err       Wald          p
      (Intercept)  3.3506037 0.3924783 72.8810823 0.00000000
      group        1.0063939 0.4045403  6.1888844 0.01285552
      Age651      -0.9554732 0.4711067  4.1133768 0.04254533
      LBBB1        0.8144717 0.4052962  4.0383780 0.04447655
      Female1     -0.3537441 0.4088817  0.7484848 0.38695637
      
      for Cost:
                    Estimate   Std.err        Wald          p
      (Intercept) 17.4930769 0.8146809 461.0599356 0.00000000
      group        2.5575322 1.0920310   5.4849449 0.01918092
      Age651      -0.3894568 1.1637970   0.1119861 0.73789423
      LBBB1       -2.7119847 1.1537917   5.5248395 0.01874832
      Female1      1.2937608 1.0126457   1.6322737 0.20138885
      

---

    Code
      NetBenReg(Followup = CEdata$survival, delta = CEdata$dead, group = CEdata$Trt,
      Cost = CEdata[, 8:22], Eff = CEdata[, 24:38], Part.times = 1:15, Method = "SW",
      Eff.only = TRUE, Cost.only = TRUE, L = 10, lambda = lambda)
    Output
      All n = 200 , Used n = 200 
      Time limit horizon L = 10 
      Censoring rate within L = 44.5 %
      Method: Simple Weighted
      
      lambda = 3:
                   Estimate  Std.err      Wald            p
      (Intercept) -7.668277 1.218434 39.608767 3.102874e-10
      group        1.893829 1.780436  1.131433 2.874699e-01
      
      for Effect:
                  Estimate   Std.err       Wald         p
      (Intercept) 3.156678 0.2954909 114.122790 0.0000000
      group       1.257365 0.4225324   8.855295 0.0029224
      
      for Cost:
                   Estimate  Std.err       Wald          p
      (Intercept) 17.138312 0.723965 560.403654 0.00000000
      group        1.878267 1.021737   3.379377 0.06601701
      

---

    Code
      NetBenReg(Followup = CEdata$survival, delta = CEdata$dead, group = CEdata$Trt,
      Cost = CEdata[, 8:22], Eff = NULL, Part.times = 1:15, Method = "SW", Z = CEdata[,
        5:7], Eff.only = TRUE, Cost.only = TRUE, lambda = lambda, L = 10)
    Message <simpleMessage>
      Eff is not provided. Assume effectiveness is survival time.
      
    Output
      All n = 200 , Used n = 200 
      Time limit horizon L = 10 
      Censoring rate within L = 44.5 %
      Method: Simple Weighted
      
      lambda = 3:
                    Estimate  Std.err       Wald         p
      (Intercept)  3.6039932 1.820554 3.91886863 0.0477470
      group        0.4041313 2.504781 0.02603187 0.8718225
      Age651       0.6306434 2.475982 0.06487432 0.7989514
      LBBB1        2.5906044 2.594926 0.99667198 0.3181171
      Female1     -1.5436565 2.271745 0.46172369 0.4968195
      
      for Effect:
                     Estimate   Std.err         Wald         p
      (Intercept)  7.03235670 0.5414582 1.686832e+02 0.0000000
      group        0.98722118 0.6679044 2.184743e+00 0.1393843
      Age651       0.08039553 0.6530719 1.515451e-02 0.9020250
      LBBB1       -0.04046009 0.6695358 3.651792e-03 0.9518131
      Female1     -0.08329857 0.5938415 1.967587e-02 0.8884461
      
      for Cost:
                    Estimate   Std.err        Wald          p
      (Intercept) 17.4930769 0.8146809 461.0599356 0.00000000
      group        2.5575322 1.0920310   5.4849449 0.01918092
      Age651      -0.3894568 1.1637970   0.1119861 0.73789423
      LBBB1       -2.7119847 1.1537917   5.5248395 0.01874832
      Female1      1.2937608 1.0126457   1.6322737 0.20138885
      

---

    Code
      NetBenReg(Followup = CEdata$survival, delta = CEdata$dead, group = CEdata$Trt,
      Cost = CEdata[, 8:22], Eff = CEdata[, 24:38], Part.times = 1:15, Method = "PT",
      Z = CEdata[, 5:7], Eff.only = TRUE, Cost.only = TRUE, lambda = lambda, L = 10)
    Output
      All n = 200 , Used n = 200 
      Time limit horizon L = 10 
      Censoring rate within L = 44.5 %
      Method: Partitioned
      
      lambda = 3:
                    Estimate  Std.err        Wald            p
      (Intercept) -8.3372754 1.186770 49.35318029 2.137845e-12
      group        0.3428794 1.607520  0.04549569 8.310952e-01
      Age651      -2.0106380 1.678660  1.43463760 2.310091e-01
      LBBB1        6.1232052 1.626024 14.18089318 1.660481e-04
      Female1     -2.0208906 1.471896  1.88508731 1.697571e-01
      
      for Effect:
                    Estimate   Std.err      Wald           p
      (Intercept)  3.0585349 0.3098642 97.428085 0.000000000
      group        0.9568928 0.3433165  7.768498 0.005316523
      Age651      -0.7166925 0.3550073  4.075595 0.043506934
      LBBB1        1.0394035 0.3431787  9.173346 0.002455652
      Female1     -0.2731597 0.3263668  0.700521 0.402608693
      
      for Cost:
                    Estimate   Std.err         Wald           p
      (Intercept) 17.5128802 0.7785548 505.98394758 0.000000000
      group        2.5277989 1.0713203   5.56732251 0.018298721
      Age651      -0.1394395 1.1524444   0.01463968 0.903695341
      LBBB1       -3.0049947 1.0844151   7.67885169 0.005587171
      Female1      1.2014116 0.9247829   1.68773452 0.193899879
      

---

    Code
      NetBenReg(Followup = CEdata$survival, delta = CEdata$dead, group = CEdata$Trt,
      Cost = CEdata[, 8:22], Eff = CEdata[, 24:38], Part.times = c(2, 2.5, 3:15),
      Method = "PT", Z = CEdata[, 5:7], Eff.only = TRUE, Cost.only = TRUE, lambda = lambda,
      L = 10)
    Output
      All n = 200 , Used n = 200 
      Time limit horizon L = 10 
      Censoring rate within L = 44.5 %
      Method: Partitioned
      
      lambda = 3:
                    Estimate  Std.err        Wald            p
      (Intercept) -7.4668365 1.147183 42.36506041 7.573098e-11
      group       -0.2143501 1.580560  0.01839184 8.921244e-01
      Age651      -2.4719607 1.668352  2.19536966 1.384260e-01
      LBBB1        6.0953429 1.604253 14.43612783 1.449939e-04
      Female1     -2.0495584 1.444478  2.01325490 1.559305e-01
      
      for Effect:
                    Estimate   Std.err       Wald           p
      (Intercept)  3.0394605 0.3128429 94.3931077 0.000000000
      group        0.9629745 0.3484861  7.6358700 0.005721878
      Age651      -0.7083864 0.3581791  3.9114689 0.047957663
      LBBB1        1.0456348 0.3480600  9.0250939 0.002662983
      Female1     -0.2932260 0.3306198  0.7865876 0.375133970
      
      for Cost:
                    Estimate   Std.err         Wald           p
      (Intercept) 16.5852179 0.8440962 386.06323006 0.000000000
      group        3.1032734 1.1320805   7.51424674 0.006121288
      Age651       0.3468014 1.1856813   0.08555113 0.769911047
      LBBB1       -2.9584384 1.1335702   6.81127197 0.009058422
      Female1      1.1698805 0.9598655   1.48546427 0.222921540
      

---

    Code
      NetBenReg(Followup = CEdata$survival, delta = CEdata$dead, group = CEdata$Trt,
      Cost = CEdata[, 8:22], Eff = CEdata[, 24:38], Part.times = 1:15, Method = "SW",
      Z = CEdata[, 5:7], interaction = c("LBBB", "Female"), Eff.only = TRUE,
      Cost.only = TRUE, lambda = lambda, L = 10)
    Output
      All n = 200 , Used n = 200 
      Time limit horizon L = 10 
      Censoring rate within L = 44.5 %
      Method: Simple Weighted
      
      lambda = 3:
                      Estimate  Std.err        Wald            p
      (Intercept)   -5.9819060 1.540572 15.07700762 0.0001032128
      group         -3.7496430 2.680647  1.95659320 0.1618781383
      Age651        -2.2708780 1.998483  1.29117911 0.2558307695
      LBBB1         -0.7141603 2.898161  0.06072205 0.8053583307
      Female1       -1.5752474 2.664696  0.34946382 0.5544168028
      group:LBBB1   10.2919725 3.630676  8.03567284 0.0045864970
      group:Female1 -0.6605186 3.547061  0.03467631 0.8522755652
      
      for Effect:
                      Estimate   Std.err        Wald            p
      (Intercept)    3.8090379 0.4566467 69.57767993 0.0000000000
      group         -0.2489669 0.6763661  0.13549386 0.7128024728
      Age651        -0.9311616 0.4458727  4.36142491 0.0367615441
      LBBB1         -0.7263065 0.5935240  1.49748762 0.2210583384
      Female1       -0.2831450 0.5752570  0.24226689 0.6225736209
      group:LBBB1    2.7265707 0.7863013 12.02418188 0.0005251474
      group:Female1  0.0883105 0.7770888  0.01291467 0.9095209989
      
      for Cost:
                      Estimate  Std.err        Wald          p
      (Intercept)   17.4090196 0.840814 428.6953432 0.00000000
      group          3.0027422 1.635463   3.3709681 0.06635475
      Age651        -0.5226068 1.220045   0.1834842 0.66839594
      LBBB1         -1.4647591 1.702119   0.7405470 0.38948571
      Female1        0.7258123 1.649879   0.1935284 0.65999604
      group:LBBB1   -2.1122604 2.217616   0.9072400 0.34084778
      group:Female1  0.9254501 2.206222   0.1759574 0.67487069
      

---

    Code
      NetBenReg(Followup = CEdata$survival, delta = CEdata$dead, group = CEdata$Trt,
      Cost = CEdata[, 8:22], Eff = NULL, Part.times = 1:15, Method = "PT", Z = CEdata[,
        5:7], interaction = c("LBBB", "Female"), Eff.only = TRUE, Cost.only = TRUE,
      lambda = lambda, L = 10)
    Message <simpleMessage>
      Eff is not provided. Assume effectiveness is survival time.
      
    Output
      All n = 200 , Used n = 200 
      Time limit horizon L = 10 
      Censoring rate within L = 44.5 %
      Method: Partitioned
      
      lambda = 3:
                       Estimate  Std.err         Wald           p
      (Intercept)    4.63366118 1.771541 6.841418e+00 0.008906811
      group         -3.42215870 3.464881 9.754918e-01 0.323314341
      Age651         1.09943385 2.221031 2.450354e-01 0.620592699
      LBBB1         -2.79819637 3.131141 7.986404e-01 0.371500189
      Female1       -0.01739779 2.657914 4.284564e-05 0.994777358
      group:LBBB1   11.07148623 4.374973 6.404145e+00 0.011385427
      group:Female1 -2.84489654 3.818562 5.550514e-01 0.456261016
      
      for Effect:
                       Estimate   Std.err         Wald          p
      (Intercept)    7.25842846 0.5685345 1.629940e+02 0.00000000
      group          0.01720975 0.9487715 3.290227e-04 0.98552798
      Age651         0.35173962 0.5742446 3.751878e-01 0.54018997
      LBBB1         -1.67237471 0.8587855 3.792257e+00 0.05149018
      Female1        0.41035124 0.7616885 2.902396e-01 0.59006703
      group:LBBB1    3.14455419 1.1576699 7.378171e+00 0.00660203
      group:Female1 -0.96042684 1.0084462 9.070330e-01 0.34090289
      
      for Cost:
                       Estimate   Std.err         Wald          p
      (Intercept)   17.14162420 0.8037831 4.548060e+02 0.00000000
      group          3.47378796 1.5612173 4.950852e+00 0.02607782
      Age651        -0.04421500 1.1789085 1.406627e-03 0.97008233
      LBBB1         -2.21892775 1.6002757 1.922634e+00 0.16556655
      Female1        1.24845152 1.4997157 6.929877e-01 0.40515002
      group:LBBB1   -1.63782366 2.0878212 6.153861e-01 0.43276704
      group:Female1 -0.03638397 2.0170049 3.253915e-04 0.98560805
      

---

    Code
      NetBenReg(Followup = CEdata$survival, delta = CEdata$dead, group = CEdata$Trt,
      Cost = CEdata[, 8:22], Eff = CEdata[, 24:38], Part.times = 1:15, Method = "SW",
      Z = CEdata[, 5:7], interaction = names(CEdata[, 5:7]), PS.Z = CEdata[, 5:7],
      Doubly.Robust = TRUE, Eff.only = TRUE, Cost.only = TRUE, lambda = lambda, L = 10)
    Output
      All n = 200 , Used n = 200 
      Time limit horizon L = 10 
      Censoring rate within L = 44.5 %
      Method: Doubly Robust Simple Weighted (estimate is causal average INB with given lambda)
      
      lambda = 3:
            Estimate  Std.err      Wald         p
      group 1.026549 1.897124 0.2927978 0.5884332
      
      for Effect:
            Estimate  Std.err     Wald           p
      group 1.079826 0.417778 6.680618 0.009746681
      
      for Cost:
            Estimate  Std.err     Wald          p
      group  2.21293 1.141833 3.756041 0.05261701
      

---

    Code
      print(fit7)
    Output
      All n = 200 , Used n = 200 
      Time limit horizon L = 10 
      Censoring rate within L = 44.5 %
      Method: Doubly Robust Partitioned (estimate is causal average INB with given lambda)
      
      lambda = 3:
             Estimate  Std.err      Wald         p
      group 0.9467823 1.595141 0.3522914 0.5528189
      
      for Effect:
            Estimate   Std.err     Wald           p
      group 1.070767 0.3357751 10.16935 0.001427945
      
      for Cost:
            Estimate  Std.err     Wald          p
      group  2.26552 1.100021 4.241642 0.03944401
      

---

    Code
      summary(fit7[[1]]$PS)
    Output
         Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
       0.2242  0.3754  0.5667  0.5350  0.7312  0.7767 

