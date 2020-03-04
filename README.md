Crowdsourcing Public Value
================

## Abstract

This exercise was designed as part of an ongoing collaboration between
INCAE Business School and Contraloría General de la República de Costa
Rica. It presents to the student with a concrete data challenge: As a
result of an open government policy, the Costa Rican Republic has made
its procurement data publicly available. The student is presented with
some context information, a brief introduction to a basic framework on
public value creation, a set of questions to help as a guide to
understand the underlying technical infrastructure allowing this
ability, and finally with a subset of data (over a hundred lines) of
public data. The dataset contains five years of information on
information technology that was acquired or rented using public funds.
The challenge is to propose to public officials concrete ways to add
value based on their analysis of these data.

## Objectives

  - Test and develop the basic skills for data management and
    exploratory data analysis
  - Recognize the benefits and drawbacks of being a power user as
    opposed to information consumers from predefined searches in a
    Business Intelligence platform
  - Recognize the challenges for Open Government Initiatives in terms of
    design, implementation and adoption

## Extra-Curricular Objectives

  - Students papers, insights and discussion will contribute to the
    advancement of current efforts to promote a data-driven discussion
    about open data in general and procurement in particular, in Costa
    Rica and beyond.

## Assumptions

The student has succesfully completed a Quantitative Methods course and
therefore understands the following subjects:

  - Utilizes visualization techniques (Tableau, Rstudio) for an
    exploratory data analysis
  - Can perform a linear regression against dummy variables to identify
    significant relationships.
  - Can perform a logistic regression to evaluate proportions when using
    categorical variables.

The student has critical thinking and written communication skills,
necessary for: \* Identify evidence (can recognize the difference
between working and critical assumptions provided the lack of data) \*
Evaluate Arguments \* Develop well-constructed conclusions.

## Contextual Information

Costa Rica has been considered one of the 20th more robust Democracies
in the world (The EIU, 2018). But as most of them, it is facing
important challenges as citizens demand more transparency, efficiency
and public participation.

According to the World Bank, “Open government is built on the idea that
citizens have the right to access government information, to actively
participate in government decisions that affect their livelihoods, and
to hold government officials and/or service providers to account when
they fail to govern properly (World Bank Group, 2017)”.

Public procurement, therefore, is a key element in any Open Data
initiative, but in this regard, the country is lacking citizen
involvement, even though, data has been available through Contraloría
General de la República. A recent assessment stated:

“The absence of an institutionalized mechanism that allows for the
participation of civil society in the public procurement system. In
general terms, there is little citizen participation and almost no
supervision from civil society over the contracting processes, and
little consideration of public opinion and of interested parties, both
in the design stages and in the contractual execution stages.” (Gobierno
de la República de Costa Rica, 2019)

Costa Rica is a member of the Open Government Partnership since 2012 and
its current action plan list 12 commitments to be accomplished by the
year 2019: “Poner a disposición de la ciudadanía los datos generados por
el SICOP en formato abierto, neutral e interoperable, siguiendo los
estándares de contratación abierta de Open Contracting Partnership”.
(Gobierno de la República de Costa Rica, 2019)

Data savvy MBA students, and future business leaders, can and should
play a role in Public or Private Value creation and this exercise
provide an opportunity to test knowledge, ingenuity and creativity.

## A Gentle Introduction to Public Value creation

Public value can be described in terms of six general types that capture
the range of possible results of government in the ways of interest…
(Harrison et al., 2011)

  - Financial – impacts on current or future income, asset values,
    liabilities, entitlements, or other aspects of wealth or risks to
    any of the above.
  - Political – impacts on a person’s or group’s influence on government
    actions or policy, on their role in political affairs, influence in
    political parties or prospects for public office.
  - Social – impacts on family or community relationships, social
    mobility, status, and identity.
  - Strategic – impacts on person’s or group’s economic or political
    advantage or opportunities, goals, and resources for innovation or
    planning.
  - Ideological – impacts on beliefs, moral or ethical commitments,
    alignment of government actions or policies or social outcomes with
    beliefs, or moral or ethical positions.
  - Stewardship – impacts on the public’s view of government officials
    as faithful stewards or guardians of the value of the government in
    terms of public trust, integrity, and legitimacy.

…Actions to effect transparency, participation, and collaboration belong
within this group of value generators. Taken as a whole, the set of
value generators consists of (Harrison et al., 2011):

  - efficiency – obtaining increased outputs or goal attainment with the
    same resources or obtaining the same outputs or goals with lower
    resource consumption.
  - effectiveness – increasing the quality of the desired outcome.
  - intrinsic enhancements – changing the environment or circumstances
    of a stakeholder in ways that are valued for their own sake.
  - transparency – access to information about the actions of government
    officials or operation of government programs that enhances
    accountability or influence on government.
  - participation – frequency and intensity of direct involvement in
    decision making about or operation of government programs or in
    selection of or actions of officials.
  - collaboration – frequency or duration of activities in which more
    than one set of stakeholders share responsibility or authority for
    decisions about operation, policies, or actions of government.

<!-- end list -->

``` r
setwd("./Tablas")
library(readxl)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
Adj_2015 <- read_excel("Reporte adjudicaciones x codigo catalogo 2015.xlsx")
Adj_2016 <- read_excel("Reporte adjudicaciones x codigo catalogo 2016.xlsx")
Adj_2017 <- read_excel("Reporte adjudicaciones x codigo catalogo 2017.xlsx")
Adj_2018 <- read_excel("Reporte adjudicacioenes x codigo catalogo 2018.xlsx")
Adj_2019 <- read_excel("Reporte adjudicacioenes x codigo catalogo 2019.xlsx")


Of_2015 <- read_excel("Reporte ofertas presentadas x codigo catalogo 2015.xlsx")
Of_2016 <- read_excel("Reporte ofertas presentadas x codigo catalogo 2016.xlsx")
Of_2017 <- read_excel("Reporte ofertas presentadas x codigo catalogo 2017.xlsx")
Of_2018_19 <- read_excel("Reporte ofertas x codigo catalogo 2018-2019.xlsx")

SIAC_15_17 <- read_excel("SIAC 2015-2017.xlsx")
SIAC_18_19 <- read_excel("ComprasSIAC.xls")

SIAC_18_19$`Año de adjudicación` <- as.numeric(SIAC_18_19$`Año de adjudicación`)

Adj_Total <- bind_rows(Adj_2015, Adj_2016, Adj_2017, Adj_2018, Adj_2019)
Of_Total <- bind_rows(Of_2015, Of_2016, Of_2017, Of_2018_19)
SIAC_Total <-bind_rows(SIAC_15_17,SIAC_18_19)

###Prepare for join 
SIAC_Total$`Clave de la línea del procedimiento`<- substr(SIAC_Total$`Clave de la línea del procedimiento`,1,10)
SIAC_Total$OBJ_GASTO <- substr(SIAC_Total$`Subpartida (COG)(AC)`,1,7)
SIAC_Total$`Subpartida (COG)(AC)`<- substr(SIAC_Total$`Subpartida (COG)(AC)`,10,100)
SIAC_Total <- SIAC_Total %>% rename(INSTITUCION = `Nombre de la entidad madre`,
                                    DESC_PROCEDIMIENTO = `Objeto contractual`,
                                    DESC_BIEN_SERVICIO = `Descripción del bien o servicio`,
                                    CEDULA_PROVEEDOR = `Cédula del adjudicatario`,
                                    NOMBRE_PROVEEDOR = `Nombre del adjudicatario`,
                                    CANTIDAD = `Cantidad licitada`,
                                    FECHA_ADJUD_FIRME = `Fecha de adjudicación`,
                                    MONTO_ADJU_CRC = `Monto adjudicados`,
                                    CED_INSTITUCION = `Clave de la línea del procedimiento`,
                                    DESC_GASTO = `Subpartida (COG)(AC)`)
SIAC_Total$CEDULA_PROVEEDOR <- as.character(SIAC_Total$CEDULA_PROVEEDOR)

Adj_Of <- bind_rows(Adj_Total, Of_Total)
Adj_Of$CAT_BIEN_SERVICIO <- ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="8111","Servicios informáticos, de computación, audio y video",
                                   ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,2)=="43","Telecomunicaciones y radiofusión de de tecnología de la información",
                                          ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="8116","Entrega de Servicios de Tecnología de Información",
                                                 ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4319","Dispositivos de comunicaciones y accesorios",
                                                        ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4320","Componentes para tecnología de la información, difusión o telecomunicaciones",
                                                               ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4321","Equipo informático y accesorios",
                                                                      ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4322","Equipos de redes de voz, multimedia, o plataformas y accesorios",
                                                                             ifelse(substr(Adj_Of$COD_BIEN_SERVICIO,1,4)=="4323","Software",NA))))))))


Adj_Of$'Año de adjudicación'<- as.numeric(paste("20",substr(Adj_Of$FECHA_ADJUD_FIRME,7,8), sep=""))
```

    ## Warning: NAs introduced by coercion

``` r
SIAC_Total$FECHA_ADJUD_FIRME <- as.character(SIAC_Total$FECHA_ADJUD_FIRME)

SIACSICOP1519  <-bind_rows(Adj_Of, SIAC_Total)
setwd('..')
```

### Methods

1.  Explain the Data Science Process (10 Minutes)
2.  Provide and explain a large dataset to be explored. Students should
    work to identify patterns in the data utilizing exploratory
    techniques. (10 minutes)
3.  Assigned by groups a 1 pager which formulates a hypothesis and
    visualizations/data to support the hypothesis. (about 10 hours)
4.  Each group presents the hypothesis. It is common that students could
    draw conclusions from the exploratory analysis. The professor can
    correct the language and remind students that the next stages of the
    Data Science process are meant to support the hypothesis with models
    and algorithms.
5.  The exploratory analysis from these sessions could be an input for
    models and algorithms to be used in practice, or higher-level
    Modeling classes.

## Short paper description

The paper should include the following:

  - An introduction
  - The findings of the exploratory data analysis that could include:
      - Exploratory visualization techniques like Pareto diagrams, bar
        charts, scatter plots
      - Causal description of the findings
  - Hypothesis for further data analysis
  - Suggestions for future analysis.

## Aditional information

Integrated Public Purchasing System Website:
<https://www.sicop.go.cr/index.jsp> Open Government Website:
<http://gobiernoabierto.go.cr/> Comptroller Office Website:
<https://www.cgr.go.cr/>

## Acknowledgements

Global MBA class of 2020 for being an engaged and enthusiastic class
that pioneered the method. Their enthusiasm and energy further motivated
future replications of this exercise.

## References

Gobierno de la República de Costa Rica. (2019). Plan de Acción de la
Alianza para un Gobierno Abierto. Retrieved from
<https://www.opengovpartnership.org/wp-content/uploads/2017/10/Costa-Rica_Action-Plan_2017-2019.pdf>
Harrison, T., Guerrero, S., Burke, G., Cook, M., Cresswell, A., Helbig,
N., … Pardo, T. (2011). Open government and e-government: Democratic
challenges from a public value perspective. In Information Polity (Vol.
17). <https://doi.org/10.1145/2037556.2037597> The EIU. (2018).
Democracy Index 2018: Me too? Political Participation, Protest and
Democracy. The Economist Intelligence Unit. World Bank Group. (2017).
Open Government Costing Framework and Methods. Retrieved from
<https://www.r4d.org/wp-content/uploads/R4D_OpenGovCF-Methods_web.pdf>

## Appendix

### Costa Rica E-Procurement Database Analysis

SICOP is an integrated, public procurement technology platform that
government institutions are obliged to use for contracting. The
following exploratory analysis makes use of two tables in the database:
1- Ofertas SICOP and 2- Adjudicaciones SICOP. This analysis focuses on
big players, namely top 10 institutions in Ofertas SICOP and
Adjudicaciones SICOP by unique count of CARTEL\_NO. The top 10 was the
same across the two tables: ICE, INS, BCCR, TSE, UCR, BCR, CNFL, COSEVI,
Radiográfica Nacional, and Contraloría General de la República. The
analysis sought to answer the following question: Is it common for
providers to get 90%-100% of the offers they present? Depending on the
number of offers involved, this could give insight into potential
corruption in the form of unjust contract allocations.

For each institution, the top 10 providers in terms of the number of
offers were analyzed (see Exhibit 1: Provider Analysis). The unique
count of CARTEL\_NO was obtained by NOMBRE\_PROVEEDOR in the Ofertas
SICOP table and in the Adjudicaciones SICOP table for each of the
institutions. The percentage of offers granted per provided was
calculated as: (CountCARTEL\_NO in Ofertas SICOP / CountCARTEL\_NO in
Adjudicaciones SICOP).

### Findings

Provider ECI TELECOM COSTA RICA got 100% of its 200 offers with ICE and
got 100% of its 23 offers with Radiografica Costarricense. Provider
HUAWEI TECHNOLOGIES COSTA RICA got 96% of its 95 offers with
Radiografica Costarricense. Even though it received only 41% of offers
made to ICE, it’s the top provider with offers to ICE and was successful
with 209 of its offers. Provider SOFTLINE INTERNATIONAL got 100% of its
120 offers with BCR.

Other cases of 100% offer success, but with lower offer count
(i.e. below 100) were PRICE WATERHOUSE COOPERS CONSULTORES in BCR,
UNISYS DE CENTRO AMERICA and SOLUCIONES TECHNOLOGICALS MUNOZ & DELGADO
in BCR, I S PRODUCTOS DE OFICINA CENTROAMERICANA in Contraloría General
de la Republica, and SOPORTE Y SERVICIO INTERNACIONAL EN REDES Y
TELECOMUNICACIONES SSIRETEL in Radiográfica Costarricense.

Since a 90%-100% success rate does not necessarily indicate corruption
in the form of unjust contract allocations, further investigations
through news sources were realized to see if some irregularities
surfaced in the news. Provider ECI TELECOM appeared in the news:
“Alleged breach of the verification duty, when awarding a contract
on-demand without competition to the provider ECI Telecom, apparently
omitting the corresponding procedure according to the amount of the
award, because it was awarded for an estimated annual demand of less
than $ 1,000,000 , and this in the years 2017, 2018 and 2019 has
registered 234 orders that accumulate an execution of $ 20.6 million as
of May 2019,” explained Sergio Olivares, Asociación Nacional de Técnicos
en Telecomunicaciones (ANTECC).1

Radiográfica Costarricense (RACSA) was discovered to be a subsidiary of
state-owned ICE and this is probably why ECI TELECOM and HUAWEI are top
providers for both. The technologies provided by ECI TELECOM COSTA RICA
are related to a HUAWEI and risks identified include: 1) uninformed
decisions that lead to band-aid fixtures, 2) disbursement of unforeseen
resources, 4) purchases that lead to idle infrastructure, 5)
concentration and excessive dependence on suppliers, and 5) weak
certification process before contracting services.2 Searches on other
providers did not raise any
    flags.

1.  <https://www.diarioextra.com/Noticia/detalle/395541/denuncian-anomalias-en-unidad-del-ice>
2.  <https://www.diarioextra.com/Noticia/detalle/395661/saturacion-amenaza-red-para-tv-paga-del-ice>

### Recommendations

This analysis only looked at the top 10 most active institutions, but
anomalies can be present throughout the entire system. SICOP auditing
standards should implement systematic controls to raise automatic alerts
for the following situations: 1) offers are made with no competition, 2)
providers are entering the 90% success rate, and 3) amounts are larger
than the category average. These alerts should initiate a revision
protocol before contracting to ensure that the correct processes were
followed. These alerts can be documented in additional columns with
simple binary code. Additionally, in order to make working with this
information more intuitive, the data dictionary should be formalized,
standardized and complemented with a process flow of the activities for
which data is collected. Definitions and nomenclature will be very
importance to facilitate and promote usage of this data.
