module str_const
implicit none

character (len=80),parameter :: A10F = 'A10F'

character (len=80),parameter :: titreA = 'Least squares software for absolute and relative gravity measurements'
character (len=70),parameter :: titreF = 'Compensation des observations de gravimetrie absolue et relative'

character (len=40),parameter :: readconfA = 'Reading configuration file'
character (len=40),parameter :: readconfF = 'Lecture du fichier de configuration'

character (len=40),parameter :: badendA = 'Program terminated with error'
character (len=40),parameter :: badendF = 'Program termine avec erreur'

character (len=40),parameter :: invalidconfA = 'Invalid configuration file'  ! 101
character (len=40),parameter :: invalidconfF = 'Fichier de configuration non valide'

character (len=40),parameter :: readcalA = 'Reading calibration file'
character (len=40),parameter :: readcalF = 'Lecture du fichier de calibration'

character (len=40),parameter :: invalidcalA = 'Invalid calibration file' ! 102
character (len=40),parameter :: invalidcalF = 'Fichier de calibration non valide'

character (len=40),parameter :: readabsA = 'Reading absolute observations files'
character (len=60),parameter :: readabsF = 'Lecture des fichiers d''observations absolues'

character (len=40),parameter :: invalidabsA = 'Invalid absolute observations file'  ! 103
character (len=60),parameter :: invalidabsF = 'Fichier de d''observations absolues non valide'

character (len=40),parameter :: readrelA = 'Reading relative observations files'
character (len=60),parameter :: readrelF = 'Lecture des fichiers d''observations relatives'

character (len=80),parameter :: invalidrelA = 'Invalid relative observations file' ! 104
character (len=80),parameter :: invalidrelF = 'Fichier d''observations relatives non valide'

character (len=80),parameter :: invalidsitefA = 'Invalid site file' ! 109
character (len=80),parameter :: invalidsitefF = 'Fichier de site non valide'

character (len=80),parameter :: invalid_cherche_staA = 'Error while creating station table' ! 105
character (len=80),parameter :: invalid_cherche_staF = 'Erreur lors de la création de la table des stations'

character (len=60),parameter :: invalid_num_staA = 'Unknown point' ! 106
character (len=60),parameter :: invalid_num_staF = 'Point inconnu'

character (len=80),parameter :: adj_impossibleA = 'Adjustment impossible. Check the SD''s in relative observations files' ! 107
character (len=80),parameter :: adj_impossibleF = 'Calcul impossible. Verifiez les ecarts-types'

character (len=80),parameter :: error_cree_diff_graviA = 'Error while creating gravity differences' ! 108
character (len=80),parameter :: error_cree_diff_graviF = 'Erreur lors de la creation des differences de gravite'

character (len=80),parameter :: error_verif_statA = 'Error while checking station names' ! 112
character (len=80),parameter :: error_verif_statF = 'Erreur lors de la vérification des stations'

character (len=80),parameter :: Cree_modeleA = 'Accumulating normal equations ...'
character (len=80),parameter :: Cree_modeleF = 'Ecriture des equations normales'

character (len=80),parameter :: not_enough_fixed_pointA = 'Not enough fixed points, back to datum-free solution...'
character (len=80),parameter :: not_enough_fixed_pointF = 'Pas assez d''observations absolues, calcul en solution libre'

character (len=40),parameter :: constraintA = 'Weighted constraint solution'
character (len=40),parameter :: constraintF = 'Calcul contraint'

character (len=40),parameter :: datumfreeA = 'Datum free solution'
character (len=40),parameter :: datumfreeF = 'Calcul libre'

character (len=40),parameter :: simulA = 'Simulation'
character (len=40),parameter :: simulF = 'Simulation'

character (len=60),parameter :: WgravA = 'Writing estimated gravities and gravimeter parameters'
character (len=60),parameter :: WgravF = 'Ecriture des gravites estimees'

character (len=60),parameter :: InversionA = 'Inverting normal matrix'
character (len=60),parameter :: InversionF = 'Inversion du systeme'

character (len=60),parameter :: detect_outliersA = 'Detecting outliers'
character (len=60),parameter :: detect_outliersF = 'Detection des fautes'

character (len=60),parameter :: stat_simulA = 'Writing SD'
character (len=60),parameter :: stat_simulF = 'Ecriture des elements statistiques'

character (len=40),parameter :: end_programA = '----- Normal end of program -----'
character (len=40),parameter :: end_programF = '----- Fin du programme -----'

character (len=60),parameter :: matrix_not_psdA = 'matrix is not positive definite' ! 110
character (len=60),parameter :: matrix_not_psdF = 'La matrice normale n''est pas definie positive'

character (len=60),parameter :: neg_diag_elemA = 'negative diagonal element '
character (len=60),parameter :: neg_diag_elemF = 'Element diagonal negatif'

character (len=60),parameter :: SDfin_nullA = 'null SD'
character (len=60),parameter :: SDfin_nullF = 'Ecart-type a posteriori nul' !111


end module str_const