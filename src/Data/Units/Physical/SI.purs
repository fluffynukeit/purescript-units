module Data.Units.Physical.SI where

-- This Units implementation is based on NIST Special Publication 811,
-- 2008 edition.

import Data.Units.Core

-- PSC currently has a potential bug in which the usual record update syntax
-- does not work, so the record are listed explicitly below.

-- * SI Base dimensions and units, Table 1
dOne = Dimension 
  { length:0, mass:0, time:0, current:0, temperature:0, amount:0, luminousIntensity:0 }
dLength = Dimension 
  { length:1, mass:0, time:0, current:0, temperature:0, amount:0, luminousIntensity:0 }
dMass = Dimension 
  { length:0, mass:1, time:0, current:0, temperature:0, amount:0, luminousIntensity:0 }
dTime = Dimension
  { length:0, mass:0, time:1, current:0, temperature:0, amount:0, luminousIntensity:0 }
dElectricCurrent = Dimension 
  { length:0, mass:0, time:0, current:1, temperature:0, amount:0, luminousIntensity:0 }
dTemperature = Dimension 
  { length:0, mass:0, time:0, current:0, temperature:1, amount:0, luminousIntensity:0 }
dAmount = Dimension 
  { length:0, mass:0, time:0, current:0, temperature:0, amount:1, luminousIntensity:0 }
dLuminousIntensity = Dimension 
  { length:0, mass:0, time:0, current:0, temperature:0, amount:0, luminousIntensity:1 }

one = std $ dOne
m = std dLength
kg = std dMass
s = std dTime
_A = std dElectricCurrent
_K = std dTemperature
mol = std dAmount
cd = std dLuminousIntensity

  
-- * SI coherent derived dimensions and units, Tables 2 and 3
dArea = dLength ^^ 2
dVolume = dLength ^^ 3
dVelocity = dLength // dTime
dAcceleration = dVelocity // dTime
dWavenumber = dLength ^^ -1
dDensity = dMass // dVolume
dSpecificVolume = dVolume // dMass
dElectricCurrentDensity = dElectricCurrent // dArea
dMagneticFieldStrength = dElectricCurrent // dLength
dLuminance = dLuminousIntensity // dArea
dConcentration = dAmount // dVolume

dPlaneAngle = dOne
dSolidAngle = dOne
dFrequency = dTime ^^ -1
dForce = dLength ** dMass // dTime ^^ 2
dPressure = dForce // dArea
dEnergy = dForce ** dLength
dPower = dEnergy // dTime
dElectricCharge = dElectricCurrent ** dTime
dElectricPotential = dPower // dElectricCurrent
dCapacitance = dElectricCharge // dElectricPotential
dElectricResistance = dElectricPotential // dElectricCurrent
dElectricConductance = dElectricResistance ^^ -1
dMagneticFlux = dElectricPotential ** dTime
dMagneticFluxDensity = dMagneticFlux // dArea
dInductance = dMagneticFlux // dElectricCurrent
dLuminousFlux = dLuminousIntensity ** dSolidAngle
dIlluminance = dLuminousFlux // dArea
dActivityRadionuclide = dTime ^^ -1
dKerma = dEnergy // dMass
dDoseEquivalent = dEnergy // dMass
dCatalyticActivity = dAmount ** dTime ^^ -1

rad = std dPlaneAngle
sr = std dSolidAngle
_Hz = std dFrequency
_N = std dForce
_Pa = std dPressure
_J = std dEnergy
_W = std dPower
_C = std dElectricCharge
_V = std dElectricPotential
_F = std dCapacitance
_O = std dElectricResistance
_S = std dElectricConductance
_Wb = std dMagneticFlux
_T = std dMagneticFluxDensity
_H = std dInductance
lm = std dLuminousFlux
lx = std dIlluminance
_Bq = std dActivityRadionuclide
_Gy = std dKerma
_Sv = std dDoseEquivalent
kat = std dCatalyticActivity

-- For convenience, some common non-base units are provided, having the same
-- dimension as base units.
gram = 1e-3 .*  kg
km = k' m
cm = c' m
mm = m' m


-- * SI Derived quantities without their own units, Table 4
dDynamicViscosity = dPressure ** dTime
dMomentOfForce = dForce ** dLength
dSurfaceTension = dForce // dLength
dAngularVelocity = dPlaneAngle // dTime
dAngularAcceleration = dPlaneAngle // dTime ^^ 2
dIrradiance = dPower // dArea
dHeatCapacity = dEnergy // dTemperature
dSpecificHeatCapacity = dEnergy // dMass // dTemperature
dSpecificEnergy = dEnergy // dMass
dThermalConductivity = dPower // dLength // dTemperature
dEnergyDensity = dEnergy // dVolume
dElecticFieldStrength = dElectricPotential // dLength
dElectricChargeDensity = dElectricCharge // dVolume
dSurfaceChargeDensity = dElectricCharge // dArea
dElectricFluxDensity = dElectricCharge // dArea
dPermittivity = dCapacitance // dLength
dPermeability = dInductance // dLength
dMolarEnergy = dEnergy // dAmount
dMolarHeatCapacity = dEnergy // dAmount // dTemperature
dExposure = dElectricCharge // dMass
dAbsorbedDoseRate = dKerma // dTime
dRadiantIntensity = dPower // dSolidAngle
dRadiance = dPower // dArea // dSolidAngle
dCatalyticActivityConcentration = dCatalyticActivity // dVolume


-- * SI prefixes, Table 5.  Prefixes operate on Quantities, which includes all 
-- units.  Prefixes all end in '.  Capitalized SI prefixes begin with _.
_Y' = (.*) 1e24
_Z' = (.*) 1e21
_E' = (.*) 1e18
_P' = (.*) 1e15
_T' = (.*) 1e12
_G' = (.*) 1e9
_M' = (.*) 1e6
k' = (.*) 1e3
h' = (.*) 1e2
da' = (.*) 1e1
d' = (.*) 1e-1
c' = (.*) 1e-2
m' = (.*) 1e-3
u' = (.*) 1e-6
n' = (.*) 1e-9
p' = (.*) 1e-12
f' = (.*) 1e-15
a' = (.*) 1e-18
z' = (.*) 1e-21
y' = (.*) 1e-24




