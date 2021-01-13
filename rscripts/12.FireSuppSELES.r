// Suppress the fire on that cell ?	
  nAccumBurnt = IF((0<LCFM <=LastDynamicClass) AND (ForestAge <= FuelThresFF)) THEN (nAccumBurnt +1) ELSE 0
  nAccumLowSprd  = IF (100*SR_noAcc <= SprdThresFF) THEN (nAccumLowSprd +1) ELSE 0
  
  // 	FUEL FIRE FIGHTING: suppressing when time since fire <= FuelThreshFF or in the front has already started local suppression
  IF (Suppressing  AND (nAccumBurnt>=AccumSuppress)) OR (FuelSuppressed)  		
  FuelSuppressed = TRUE
  burning = FALSE
  // SPREAD FIRE FIGHTING: suppressing when time since fire <= SpreadThreshFF or in the front has already started local suppression
  ELSE IF (Suppressing  AND (nAccumLowSprd>=AccumSuppress)) OR (SprdSuppressed)
  SprdSuppressed = TRUE	
  burning = FALSE
  ENDFN
