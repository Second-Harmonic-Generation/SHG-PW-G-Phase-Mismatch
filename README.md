## 🧰 How to Use This Template    

Click the green **"Use this template"** button at the top of the page, then choose **"Create a new repository"**.   

This will create your own copy of this project, which you can modify freely — no need to fork!   

 
<p align="center">
  <img src="./images/SHG-banner.png" alt="SHG Logo">
</p>


<h1 align="center">SHG-PW-G-Phase-Mismatch</h1>

<div align="center">

| **Term** | **Definition** |
|----------|----------------|
| **SHG** | Second Harmonic Generation |
| **PW** | Pulsed Wave |
| **G** | Gaussian |
</div>

&nbsp;

<div align="center">

Article title:       
**Thermally Induced Phase Mismatching in a Repetitively Gaussian Pulsed Pumping KTP Crystal: a Spatio-Temporal Treatment**
</div>

&nbsp;

---

***Table of Contents***

<div>
  &nbsp;&nbsp;&nbsp;&nbsp;<a href="#1-about-this-repository"><i><b>1. About this repository</b></i></a>
</div>
&nbsp;

<div>
  &nbsp;&nbsp;&nbsp;&nbsp;<a href="#2-getting-started"><i><b>2. Getting Started</b></i></a>
</div>
&nbsp;

<div>
  &nbsp;&nbsp;&nbsp;&nbsp;<a href="#3-how-to-cite-us"><i><b>3. How to Cite Us</b></i></a>
</div>
&nbsp;


<div>
  &nbsp;&nbsp;&nbsp;&nbsp;<a href="#4-contact-information"><i><b>4. Contact Information</b></i></a>
</div>
&nbsp;

---    

## 1. About this repository

This repository contains the **Toolkit for Modeling Thermally Induced Phase Mismatching in KTP Crystal: Pulsed-Wave Gaussian Second Harmonic Generation**, an open-source toolkit for modeling the spatiotemporal phase mismatching effects that occur in pulsed-wave second-harmonic generation (PW SHG), using KTP as a case study.

### Toolkit Overview

The toolkit provides comprehensive modules for modeling thermally induced phase mismatching (TIPM) in KTP crystals under repetitively pulsed Gaussian pumping, including spatiotemporal temperature field computation and phase mismatch calculations.

The toolkit implements a numerical procedure for solving repetitively pulsed pumped crystals using finite difference methods, enabling computation on home-used computing machines. It supports parameterized scenario sweeps including temperature-dependent versus constant thermal conductivity, realistic cooling mechanisms (conduction, convection, and radiation), and heat-transfer coefficients spanning 6.5–2.0×10⁴ W·m⁻²·K⁻¹. The toolkit features compiled Fortran kernels with built-in benchmark reporting, reproducible pipelines with versioned code repository, and exportable datasets with spatiotemporal temperature and phase mismatch fields. It generates both radial and axial profiles for temperature and phase mismatch analysis.

The implementation has been validated by reproducing phase mismatching behaviors and trends for KTP under repetitively pulsed Gaussian pumping, including the effects of temperature-dependent conductivity and boundary conditions. The toolkit demonstrates accumulative behaviors of temperature and TIPM (with its reverse sign) when the number of pulses is increased, along with fluctuations attributed to the offtime between successive pulses. This toolkit was used to solve the phase mismatch modeling problem described in the research article **"Thermally Induced Phase Mismatching in a Repetitively Gaussian Pulsed Pumping KTP Crystal: a Spatio-Temporal Treatment"** (DOI: 10.1364/AO.54.004781, Applied Optics, 2015).  

### Directory Structure

```
Folder PATH listing
+---citation                      <-- Research papers and documentation
│       1_Heat-Equation_Continu…  <-- Heat equation continuous wave analysis
│       2_Heat-Equation_Continu…  <-- Heat equation continuous wave study
│       3_Heat-Equation_Pulsed-…  <-- Heat equation pulsed wave analysis
│       4_Phase-Mismatch_Pulsed…  <-- Phase mismatch pulsed wave study
│       5_Ideal_Continuous-Wave…  <-- Ideal continuous wave analysis
│       6_Ideal_Pulsed-Wave_Bes…  <-- Ideal pulsed wave Bessel-Gaussian
│       7_Coupled_Continuous-Wa…  <-- Coupled continuous wave analysis
│       README.md                 <-- Citation guidelines and references
│
+---images                        <-- Visual assets and graphics
│       SHG-banner.png            <-- Project banner image
│
+---results                       <-- Output data and analysis results
│       E_009_f_500_Np_1_tp_50…   <-- Phase mismatch radial profile data
│       E_009_f_500_Np_1_tp_50…   <-- Phase mismatch transverse profile
│       E_009_f_500_Np_1_tp_50…   <-- Phase mismatch axial profile data
│       E_009_f_500_Np_1_tp_50…   <-- Temperature radial profile data
│       E_009_f_500_Np_1_tp_50…   <-- Temperature transverse profile
│       E_009_f_500_Np_1_tp_50…   <-- Temperature axial profile data
│
+---src                           <-- Source code and implementation
│       Code_SHG-PW-G-Phase-Mi…   <-- Main Fortran implementation
│
│       .Zone.Identifier          <-- Windows security zone identifier
│       Article_SHG-PW-G-Phase…   <-- Main research article PDF
│       CITATION.cff              <-- Citation metadata file
│       LICENSE                   <-- Project license information
│       README.md                 <-- Project documentation
```

## 2. Getting Started

### 2.1. Prerequisites
- **Fortran Compiler** (gfortran, Intel Fortran, or similar)
- **Text Editor** (VS Code, Cursor, or any Fortran-capable editor)
- **PDF Reader** (for accessing research papers)
- **Git** (for cloning the repository)

### 2.2. Quick Start

1. **Clone the repository**
   ```bash
   git clone https://github.com/Second-Harmonic-Generation/SHG-PW-G-Phase-Mismatch.git
   cd SHG-PW-G-Phase-Mismatch
   ```

2. **Explore the Research Papers**
   - Navigate to the `citation/` folder
   - Read the main research paper: `Article_SHG-PW-G-Phase-Mismatch.pdf`
   - Review additional papers for comprehensive understanding

3. **Compile and Run the Toolkit**
   ```bash
   cd src
   gfortran -o phase_mismatch Code_SHG-PW-G-Phase-Mismatch.f90
   ./phase_mismatch
   ```

4. **Analyze Results**
   - Check the `results/` folder for output files and benchmark data
   - Examine `.plt` files for both temperature and phase mismatch profiles (radial, transverse, axial)
   - Use plotting software (Gnuplot, Python matplotlib, etc.) to visualize results


---


## 3. How to Cite Us
Please refer to the [**citation**](./citation/) folder for accurate citations. It contains essential guidelines for accurate referencing, ensuring accurate acknowledgement of our work.


  
## 4. Contact Information

For questions not addressed in the resources above, please connect with [Mostafa Rezaee](https://www.linkedin.com/in/mostafa-rezaee/) on LinkedIn for personalized assistance.
