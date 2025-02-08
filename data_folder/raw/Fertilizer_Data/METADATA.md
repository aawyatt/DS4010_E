# COBS NPK fertilizer data
This dataset contains information on the application of nitrogen (N), phosphorus (P), and potassium (K) fertilizers across different cropping systems in the Comparison of Biofuel Systems (COBS) experiment conducted at Iowa State University. The dataset records fertilizer application rates for N, P, and K in Kgs/ha, crop treatment types, and associated dates.

## Data Dictionary:

### **Date**  
- **Description:** Date of fertilizer application  
- **Type:** Date  
- **Format:** mm/dd/yy  
- **Missing Values:** Treatment **P** (unfertilized prairie) has no date associated because no fertilizer was applied  

### **Year**  
- **Description:** Year the data is from  
- **Type:** Categorical  
- **Range:** 2009 - 2022  
  

### **Treatment**  
- **Description:** Treatment applied  
- **Type:** Categorical Variable  
- **Treatment Categories:**  
  - **CC** = Continuous corn without a cover crop  
  - **CCW** = Continuous corn with a rye cover crop  
  - **P** = Unfertilized prairie  
  - **PF** = Fertilized prairie  


### **N kg/ha**  
- **Description:** Nitrogen applied per hectare  
- **Type:** Float  
- **Range:** Non-negative Real
- **Units:** Kg/Ha
  

### **P kg/ha**  
- **Description:** Phosphorus applied per hectare  
- **Type:** Float  
- **Range:** Non-negative Real
- **Units:** Kg/Ha

### **K kg/ha**  
- **Description:** Potassium applied per hectare  
- **Type:** Flaot  
- **Range:** Non-negative Real
- **Units:** Kg/Ha
  
