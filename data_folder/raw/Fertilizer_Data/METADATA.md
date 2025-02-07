# COBS NPK fertilizer data
This dataset contains information on the application of nitrogen (N), phosphorus (P), and potassium (K) fertilizers across different cropping systems in the Comparison of Biofuel Systems (COBS) experiment conducted at Iowa State University. The dataset records fertilizer application rates for N, P, and K in Kgs/ha, crop treatment types, and associated dates.

## Variables:
- **Date:** Date for application of Fertilizers. 

   Type: Date
   Format: mm/dd/yy
   Missing Values: Treatment P doesn't have a date associated because it is the unfertilized praire.
  
- **Year:** Year the data is from.
  Type: Categorical
  Range: 2009-2022
  
- **Treatment:** Treatment applied.
  Type: Categorical Variable
  Treatment Categories: CC: continuous corn without a cover crop; CCW: Continuous corn with a rye cover crop; P: unfertilized prairie; PF: fertilized prairie.
  
- **N kg/ha:** Nitrogen applied in Kg/Ha
  Type: Float
  
- **P kg/ha:** Phosphorus applied in Kg/Ha
  Type: Float
  
- **K kg/ha:** Potassium applied in Kg/Ha
  Type: Float
  
