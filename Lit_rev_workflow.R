
install.packages("DiagrammeR")
install.packages("rsvg")
install.packages("DiagrammeRsvg")

library(DiagrammeR)
library(rsvg)
library(DiagrammeRsvg)

plot <- grViz("
digraph flowchart {
  node [shape=box, style=filled, fillcolor=lightblue];

  A [label='Choose a research category']
  A1 [label='Reproductive Behaviour']
  A2 [label='Population Genetics']
  A3 [label='Demography']
  A4 [label='Social Behaviour']
  B [label='Are samples collected opportunistically from litters']
  B1 [label='Yes']
  B2 [label='No']
  C [label='Are there > 10 litters collected?']
  C1 [label='Yes']
  C2 [label='No']
  C3 [label='Go get more']
  C4 [label='Is the total sample size sufficient to be biologically meaningful?']
  C5 [label='Yes - proceed to sequencing']
  C6 [label='No - go get more']
  D [label='Marker Choice']
  D1 [label='Microsatellites']
  D2 [label='SNPs']
  E [label='Categorical Estimator']
  E2 [label='Categorical Estimator']
  E3 [label='Continuous Estimator']
  F [label='COLONY']
  F1 [label='CERVUS']
  F2 [label='GERUD']
  F4 [label='EMIBD9']
  F5 [label='COANCESTRY']
  F6 [label='COLONY']
  F7 [label='SEQUOIA']
  G [label='Population Connectivity']
  G1 [label='Allendorf Phelps Effect']
  G2 [label='Have there been prior studies conducted on structure?']
  G3 [label='Yes']
  G4 [label='No']
  G5 [label='Sample within relevant population boundaries']
  G6 [label='Collect samples with broad geographic coverage']
  G7 [label='Is the total sample size > 100']
  G8 [label='Yes - proceed to sequencing']
  G9 [label='No - get back in the field']
  G10 [label='Marker Choice']
  G11 [label='Microsatellites']
  G12 [label='SNPs']
  G16 [label='KING']
  G17 [label='SNP-Relate']
  G18 [label='Categorical']
  G19[label='Continuous']
  G20[label='Categorical']
  G21[label='COLONY']
  G22[label='CERVUS']
  G23[label='COANCESTRY']
  G24[label='EMIBD9']
  G25[label='COLONY']
  G26[label='SEQUOIA']
  G27 [label = 'CKMRsim']
   H [label='What population estimates are you trying to produce?']
   H1[label='Single population']
   H2[label='Broad-scale']
   H3 [label='Sample within relevant population boundaries']
  H4 [label='Collect samples with broad geographic coverage']
   H5 [label='Is the sample size > 100 for each population']\
   H6 [label='Yes - proceed to sequencing']
  H7 [label='No - get back in the field']
  H8 [label='SNPs']
  H9 [label='CKMRsim']
  H10 [label='Kinference']
  H11 [label='Kin Finding']
  H12 [label='Categorical']
  SOC [label='Does the study species form aggregations?']
  SOC1 [label='Yes']
  SOC2 [label='No']
  SOC3 [label='Sample aggregations and randomly within population']
  SOC4 [label='Sample randomly within population']
  SOC5 [label='Is the total sample size > 100']
  SOC6 [label='Yes - proceed to sequencing']
  SOC7 [label='No - get back in the field']
  SOC8 [label='Marker Choice']
  SOC9 [label='Microsatellites']
  SOC10 [label = 'SNPs']
  SOC11 [label='Kin finding']
  SOC111 [label = 'Kin finding']
  SOC12 [label = 'Continuous Estimator']
  SOC13 [label = 'Categorical Estimator']
  SOC14 [label = 'COANCESTRY']
  SOC15 [label = 'EMIBD9']
  SOC16 [label = 'COLONY']
  SOC17 [label = 'SEQUOIA']
  SOC18 [label = 'CKMRsim']

  A -> A1;
  A -> A2;
  A -> A3;
  A -> A4;
  A1 -> B;
  B -> B1;
  B -> B2;
  B1 -> C;
  C -> C1;
  C -> C2;
  C1 -> C4;
  C2 -> C3;
  B2 -> C4;
  C4 -> C5;
  C4 -> C6;
  C5 -> D;
  D -> D1;
  D -> D2;
  D1 -> E;
  D2 -> E2;
  D2 -> E3;
  E -> F;
  E -> F1;
  E -> F2;
  E3 -> F4;
  E3 -> F5;
  E2 -> F6;
  E2 -> F7;
  A2 -> G;
  A2 -> G1;
  G -> G2;
  G2 -> G3;
  G2 -> G4;
  G3 -> G5;
  G4 -> G6;
  G5 -> G7;
  G7 -> G8;
  G7 -> G9;
  G8 -> G10;
  G10 -> G11;
  G10 -> G12;
  G11 -> G18;
  G12 -> G19;
  G12 -> G20;
  G18 -> G21;
  G18-> G22;
  G19 -> G16;
  G19 -> G17;
  G19 -> G23;
  G19 -> G24;
  G20 -> G25;
  G20 -> G26;
  G20 -> G27;
  G1 -> G5;
  A3 -> H;
  G6 -> G7;
  H -> H1;
  H -> H2;
  H1 -> H3;
  H2 -> H4;
  H3 -> H4;
  H4 -> H5;
  H5 -> H6;
  H5 -> H7;
  H6 -> H8;
  H8 -> H11;
  H11 -> H12;
  H12 -> H9;
  H12 -> H10;
  
  A4 -> SOC;
  SOC -> SOC1;
  SOC -> SOC2;
  SOC1 -> SOC3;
  SOC2 -> SOC4;
  SOC3 -> SOC5;
  SOC4 -> SOC5;
  SOC5 -> SOC6; 
  SOC5 -> SOC7;
  SOC6 -> SOC8;
  SOC8 -> SOC9;
  SOC8 -> SOC10;
  SOC9 -> SOC111;
  SOC10 -> SOC11;
  SOC11 -> SOC12;
  SOC11 -> SOC13;
  SOC12 -> SOC14;
  SOC12 -> SOC15;
  SOC13 -> SOC16; 
  SOC13 -> SOC17
  SOC13 -> SOC18
  
}
")

plot


plot %>%
  export_svg() %>%
  charToRaw %>% 
  rsvg_pdf("C:/Users/samue/Desktop/Honours/Chapter_1_lit_review/New_Plots/plot.pdf")
