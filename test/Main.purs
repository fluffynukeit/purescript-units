module Test.Main where

import Debug.Trace
import Data.Units.Core
import Data.Units.Physical.SI
import Data.Either

hr = 3600 .* s

main = do
  let mySpeed = 13 @+ m/s
      myMass = 75 @+ kg
      myCommute = 15 @+ km :: Quantity _ _ String -- String context
      myCommuteTime = myCommute / mySpeed 
      myCommuteTimeInHrs = myCommuteTime @- hr
      myCommuteTimeInVolts = myCommuteTime @- _V
      myKE = 1/2 .* myMass * mySpeed ^. 2
      myCalcSpeed = myKE/myMass *. 2 # root 2
      nonsense = myKE + myMass - (myCommute + s) * myMass
      nonsenseCtx = qCtx "Nonsense" nonsense

  trace $ "Commute time in sec: " ++ show (myCommuteTime @- s)
  trace $ "Commute time in hrs: " ++ show myCommuteTimeInHrs
  trace $ "Commute time in volts: " ++ show myCommuteTimeInVolts
  trace $ "My kinetic energy in J: " ++ show (myKE @- _J)
  trace $ "My calculated speed in m/s: " ++ show (myCalcSpeed @- m/s)
  trace $ ""
  trace $ "Nonsense calculation in C: " ++ show (nonsense @- _C)
  trace $ "Nonsense with context in C: " ++ show (nonsenseCtx @- _C)

   
