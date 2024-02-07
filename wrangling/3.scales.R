# using afe, find good number of scales.
# then, use factor loadings on scales to create each axis

# Make scales (REDO) -------------------------------------------------------------

CleanData$scale_nationaSouv <-   (CleanData$iss_nationalisme_qcRightDirection+
                                    CleanData$iss_nationalisme_souv+
                                    CleanData$iss_nationalisme_qcBefCan)/3
hist(CleanData$scale_nationaSouv)


CleanData$scale_langFr <- (CleanData$drop_frenchDanger +
                             CleanData$drop_worriedFrMtl +
                             CleanData$drop_worriedFrProv +
                             CleanData$drop_englishCegep +
                             CleanData$drop_businessFrench +
                             CleanData$drop_afraidDisappear)/6
hist(CleanData$scale_langFr)


CleanData$scale_laicite <-   (CleanData$iss_laic_relSignsTeachersNo +
                                CleanData$iss_laic_relSignsWorkNo +
                                CleanData$iss_laic_religionImportant +
                                CleanData$iss_laic_secularismEncouraged)/4
hist(CleanData$scale_laicite)

CleanData$scale_immigration <-   finverser((CleanData$iss_immig_immAdapt+
                                              CleanData$iss_immig_immBenefit+
                                              CleanData$iss_immig_immLearnFr+
                                              CleanData$iss_immig_immLess+
                                              CleanData$iss_immig_immThreat)/5)
hist(CleanData$scale_immigration)

CleanData$scale_woke <-   (CleanData$iss_newleft_wokeWhiteRac +
                             CleanData$iss_newleft_wokenoWhites+
                             CleanData$iss_newleft_wokeCensor+
                             CleanData$iss_newleft_wokeSocCtrl+
                             CleanData$iss_newleft_wokeRich+
                             CleanData$iss_newleft_wokeSysRaci+
                             CleanData$iss_newleft_wokeWhiteMenFav+
                             CleanData$iss_newleft_wokeArtists)/8
hist(CleanData$scale_woke)

CleanData$scale_3elien <- (CleanData$iss_3elien_Accord+CleanData$iss_3elien_Dim)/2
hist(CleanData$scale_3elien)

CleanData$scale_enviro <- (CleanData$drop_envGvtMore+CleanData$drop_envMeat+
                             CleanData$drop_envLifestyle+CleanData$drop_envTransp)/4