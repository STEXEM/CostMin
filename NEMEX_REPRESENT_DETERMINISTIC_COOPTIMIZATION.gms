$Title Network and generation EXpansion planning in the Electricity Market (NEXEM)

$OnText

Developed by

   Isaac Camilo González
   Instituto de Inestigación Tecnológica
   isaac.gonzalez@iit.comillas.edu
   Septiembre, 2017

$OffText

$OnEmpty OnMulti OffListing Oneps
$ifthen                       %gams.user2% == ""
$setglobal OptSkipExcelInput  0
$else
$setglobal OptSkipExcelInput  %gams.user2%
$endif
$ifthen                       %gams.user3% == ""
$setglobal OptSkipExcelOutput 0
$else
$setglobal OptSkipExcelOutput %gams.user3%
$endif

$OnEmpty OnMulti OffListing


* definition of symbol for comments at the end of the line
$EOLCOM //

* optimizer definition
OPTION   lp = cplex ;
OPTION  mip = gurobi ;
OPTION rmip = cplex ;

* general options
OPTION optcr    =  0.00     ;   // tolerance to solve MIP until IntGap < OptcR
OPTION reslim   = 129600 ;   // maximum run time [sec]
OPTION threads  =     -1 ;   // number of cores
OPTION solprint =     on ;   // print the final solution in the .lst file
OPTION limrow   =   1000 ;   // maximum number of equations in the .lst file
OPTION limcol   =   1000 ;   // maximum number of variables in the .lst file


sets
* Temporal Indixes
   p           periods
   pa(p)       active periods
   ps(p)       window periods
   pt(p)       all periods
   rp          representative periods
   rpp(rp,p)   relation between representative periods and periods
   y           year
   z (y)       year
   b           block
   p1(p)       first block
   pn(p)       last block
   s           slice
   n1(s)       first block
   nn(s)       last block
   IndexRP (p,p)

* Generators Indixes
   th          technologies
   g           generating   unit
   t (g)       thermal      unit
   h (g)       hydro plant  unit
   sl(g)       solar unit
   wn(g)       wind unit
   hr(g)       Hydraulic (or storage) Generators with        reservoir
   hf(g)       Hydraulic (or storage) Generators with "fast" reservoir (i.e. battery)
   hs(g)       Hydraulic (or storage) Generators with "slow" reservoir (i.e. hydro  )
   w (g)       renewables   unit
   gc(g)       candidate unit

* Network Related indexes
   d           node (bus)
   lc (d,d)    candidate lines
   la (d,d)    existing  and   candidate lines
   le (d,d)    existing  lines
   ged(g,d)    existing  unit at  a node
   gnd(g,d)    all posible   unit at  a all nodes
   gcd(g,d)    candidate unit at  a node
   gad(g,d)    existing + candidates lines

   tg(g,th)    relation betwwen generator and technology

alias (d,di,df)   ;
alias (rp,rrpp)   ;
alias (p,pp,ppp)   ;



parameters
*Generation parameters

   pENSCost                      energy non-served cost             [M€ per GWh]
   pPNSCost                      power  non-served cost             [M€ per GW ]
   pSlopeVarCost (      g    )   slope     variable cost            [M€ per GWh]
   pInterVarCost (      g    )   intercept variable cost            [M€ per   h]
   pStartupCost  (      g    )   startup            cost            [M€]
   pGenInvCost   (      g    )   fixed    cost                      [M€]
   pMaxProd      (      g    )   maximum output                     [GW]
   pMinProd      (      g    )
   pEFOR         (      g    )   EFOR                               [p.u.]
   tSolar        ( p, g      )
   pMaxProdSolar ( p, g      )
   pWindGen      ( p,   g    )
   pEmissionRate (th)
   pCO2price


*Demand parameters
   pDuration     (  p,s      )   duration block b- slice s          [h]
   pDemand       (  p,s      )   hourly load by node                [GW]
   pDemIncr      (y          )   yearly demand increment            [p.u.]
   pCumDemIncr   (y          )   cum yearly demand increment        [p.u.]
   pOrder        (y          )   ordinal of the year                [p.u.]

*Results Parameter
   pInstalLine   (y,      d,d)   Lines Installed                    [p.u.]
   pCommit       (y,p,s,g,  d)   Units Commited                     [p.u.]
   pCommit_can   (y,p,s,g,d  )   Units Commited                     [p.u.]
   pInstalCapT   (      g,d  )   Generation capacity installed      [p.u.]
   pInstalCapT_can(y,   g,d  )   Generation capacity installed      [p.u.]
   pProduct      (y,p, g      )
   pProductT     (y,p    ,th  )
   pAnnualEnergy (y     ,  th  )
   pProduct_all  (y,p,th      )
   pFlow         (y,p,s,  d,d)   Flows through lines                [GW]
   pTheta        (y,p,s,    d)   Angles                             [rad]
   pReserve      (y,p,    g,d )
   pInflows      (  p,  g    )   inflows                            [km3]
   pLRMC         (y,p,      d)   locational marginal prices
   pStartup      (y,p,s,g,d   )   start up decision                  [p.u.]
   pENS          (y,  s,    d)   energy non served                  [GW-h]
   pTotalCost                    total investment and operation cost[M€]

*Network Parameters
   pX            (        d,d)   line reactance                     [p.u.]
   pTTC          (        d,d)   total transfer capacity            [GW]
   pFixedCost    (        d,d)   fixed    cost                      [M€]
   pDemShare     (          d)   demand share                       [p.u.]
   pSbase                        base power                         [GW]

*Storage Parameters
   pMaxReserve   (      g    )   maximum reserve                    [km3]
   pMinReserve   (      g    )   minimum reserve                    [km3]
   pIniReserve   (      g    )   initial reserve                    [km3]
   pProdFunct    (      g    )   production function                [GWh per km3]
   pEffic        (      g    )   pumping efficiency                 [p.u.]
   pMaxCons      (      g    )   Maximum consuption of pump units
   pFinReserve   (      g    )

* Parameters for representatives periods formulation
   pWeight_rp      (rp   )       Representatives periods weight [h]
   pNumPer_rp      (rp   )       Number of periods at each representative period [h]
   pFirstP_rp      (rp,p )       First period of each representative period
   pTransMatrix_rp (rp,rp)       Transition matrix representing number of jumps from rp to rrpp
   pNumPerMinus1_rp(rp   )       Number of periods minus 1 at each representative period [h]
   pMaxTM_rp                     Maximum value of representative period transition matrix
   pMinTransition                Minimum transition to be considered in RP_TM model
   pStorMovWindow                Storage moving window for representative periods [h]
   pLastStorMovW                 parameter to indicate the last hour on ps(p) set

*Excecution Parameters
   pModel                        Model {1-completo 2-relajado 3-simple}
   pSearch                       1- all 2- candidate
   OF_Cost    (*  )    Total cost model                   [k€]
   GenCPUTime (*  )    Generation CPU time                [ s]
   SolCPUTime (*  )    Solve      CPU time                [ s]
   NumVar     (*  )    Number of variables
   NumDVar    (*  )    Number of discrete variables
   NumEqu     (*  )    Number of equations
   NumNZ      (*  )    Number of nonzero entries in the model coefficient matrix
   BestSol    (*  )    The estimate of the best possible solution for a MIP

binary   variables
**** GEP variables ****

*all possible locations
   vNewGen       (y,    g,d  )   cumulative installation decision   [0-1]
   vCommitt      (y,p,s,g,d  )   commitment of the unit             [0-1]
   vStartup      (y,p,s,g,d  )   startup     of the unit            [0-1]
   vShutdown     (y,p,s,g,d  )   shutdown    of the unit            [0-1]

*candidate generators
   vNewGen_can   (y,    g,d    )   cumulative installation decision   [0-1]
   vCommitt_can  (y,p,s,g,d    )   commitment of the unit             [0-1]
   vStartup_can  (y,p,s,g,d    )   startup     of the unit            [0-1]
   vShutdown_can (y,p,s,g,d    )   shutdown    of the unit            [0-1]

**** TEP variables ****
   vNewLine      (y,      d,d)   line investment decision           [0-1]

positive variables
**** GEP variables ****
*common to all models
   vReserve      (y,p,g,d      )   reserve at the end of period       [km3]
   vSpillage     (y,p,g,d      )   spillage                           [km3]
*Candidates Units
   vConsump      (y,p,s,g,d  )   consumption of the unit              [GW]
   vProduct      (y,p,s,g,d  )   production  of the unit              [GW]
*all possible units
   vProduct_all  (y,p,s,g,d  )   production  of the unit per node   [GW]
   vConsump_all  (y,p,s,g,d  )
   vENS          (y,p,s,    d)   energy non served                  [GW]
*all possible units relaxed
   vNewGen_R     (y,    g,d  )   cumulative installation decision   [GW]
   vCommitt_R    (y,p,s,g,d  )   commitment of the unit             [p.u.]
   vStartup_R    (y,p,s,g,d  )   startup     of the unit            [p.u.]
   vShutdown_R   (y,p,s,g,d  )   shutdown    of the unit            [p.u.]
*candidate units relaxed
   vNewGen_R_can (y,    g,d  )   cumulative installation decision   [GW]
   vCommitt_R_can(y,p,s,g,d  )   commitment of the unit             [p.u.]
   vStartup_R_can(y,p,s,g,d  )   startup     of the unit            [p.u.]
   vShutdwn_R_can(y,p,s,g,d  )   shutdown    of the unit            [p.u.]

variables
   vFlow         (y,p,s,  d,d)   flow                               [GW]
   vTheta        (y,p,s,    d)   voltage angle                      [rad]

variables
   vTotalTCost                   total system          cost         [M€]
   vTotalFCost                   total system fixed    cost         [M€]
   vTotalVCost                   total system variable cost         [M€] ;

equations

* C = Complete: It includes Unit Commitment Constraints (UCC)
* S = Simple: It does not include UCC
* R = Relaxed: It relaxes UCC
* Prefix ALL = all possible siting options
* Prefix can = candidate siting options

   E_ACOP_RP      (y,rp,p,rp,s,g,d    )


   eTotalTCost                   total          system          cost[M€]

   eTotalFCost_C_can             total complete system fixed    cost[M€]
   eTotalFCost_R_can             total relaxed  can  fixed cost     [M€]

   eTotalVCost_C_can             total complete system variable cost[M€]
   eTotalVCost_S_can             total simple   system variable cost[M€]
   eTotalVCost_R_can             total relaxed  can variable cost   [M€]


   eBalance_C_all (y,p,s,  d  )  load generation balance            [GW]
   eBalance_C_can (y,p,s,  d  )  load generation balance            [GW]
   eBalance_R_all (y,p,s,  d  )  load generation balance            [GW]

   eMaxProd_C_can_0(y,p,s,g,d )  max prod some candidates C         [GW]
   eMaxProd_C_can_1(y,p,s,g,d )  max prod some candidates C         [GW]
   eMaxProd_C_can_2(y,p,s,g,d )  max prod some candidates C         [GW]
   eMaxProd_C_can_3(y,p,s,g,d )  max prod some candidates C         [GW]
   eMaxCons_C_can_1(y,p,s,g,d )

   eMaxProd_R_can_1(y,p,s,g,d )  max prod some candidates R         [GW]
   eMaxProd_R_can_2(y,p,s,g,d )  max prod some candidates R         [GW]
   eMaxProd_R_can_3(y,p,s,g,d )  max prod some candidates R         [GW]
   eMaxCons_R_can_1(y,p,s,g,d )  max cons some candidates R         [GW]

   eMinProd_C_can_1(y,p,s,g,d )  min prod some candidates C         [GW]
   eMinProd_C_can_2(y,p,s,g,d )  min prod some candidates C         [GW]

   eMinProd_R_can_1(y,p,s,g,d )  min prod some candidates R         [GW]
   eMinProd_R_can_2(y,p,s,g,d )  min prod some candidates R         [GW]
   eMinProd_R_can_3(y,p,s,g,d )

   eMinReserve     (y,p,g,d )
   eMaxReserve     (y,p,g,d )
   eMinSpillage    (y,p,g,d )
   eMaxSpillage    (y,p,g,d )


   eGenInst_C_can (y,    g, d )  Cumulative generation installation [p.u.]
   eGenInst_R_can (y,    g, d )  Cumulative generation installation [p.u.]

   eStorage_C_can (y,p,g,d    )  storage balance equation

   eMaxInst_C_can (y,    d  )  Maximun units intall pero type g   [p.u.]
   eMaxInst_R_can (y,    d    )  Maximun units intall pero type g   [p.u.]

   eStrtUpPer_C_can (y,p,s,g,d )
   eStrtUpPer_C_can2(y,p,s,g,d )
   eStrtUpPer_R_can (y,p,s,g,d )


   eCumLineInstall(y,      d,d)  Cumulative lines installation
   eFlowInstlCap1 (y,p,s,  d,d)  Max flow for installed capacity    [GW]
   eFlowInstlCap2 (y,p,s,  d,d)  Max flow for installed capacity    [GW]
   eFlowNetN1     (y,p,s,  d,d)
   eFlowNetN2     (y,p,s,  d,d)
   eFlowNetEx     (y,p,s,  d,d)  flow for each existing  line       [GW]

   E_eStrtUpPer_C_can_RP (y,rp,p,rp,s,g,d )

   eRSRVH_RP1_C_CAN(y,p,g,d     ) storage with representative days
   eRSRVH_RP2_C_CAN(y,p,g,d     ) storage with representative days

;

*--------------------------- Objective Function --------------------------------

eTotalTCost       .. vTotalTCost  =e= vTotalFCost + vTotalVCost ;

* ----------------- Complete_ candidate  generations units --------------------

eTotalFCost_C_can                    .. vTotalFCost =e=
                                        sum[(y,gcd(g,d)),(card(y)-ord(y)+1)*pGenInvCost (g)*[vNewGen_can(y, g,d) - vNewGen_can(y-1, g,d)]]+
                                        sum[(y,lc     ), (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine(y,lc) - vNewLine(y-1,lc)]]      ;
eTotalVCost_C_can                    .. vTotalVCost  =e=
                                        sum[(y,rpp(rp,pa(p)),s,  d), pWeight_rp(rp)*pENSCost        *  vENS        (y,p,s,  d)] +
*                                       sum[(y,p    ,s    ),                pPNSCost        *  vPNS        (y,p,s    )] +
                                        sum[(y,rpp(rp,pa(p)),s,gad(t,d)), pWeight_rp(rp)*pStartupCost (t)*  vStartup_can(y,p,s,t,d)] +
                                        sum[(y,rpp(rp,pa(p)),s,gad(t,d)), pWeight_rp(rp)*pInterVarCost(t)*  vCommitt_can(y,p,s,t,d)] +
                                        sum[(y,rpp(rp,pa(p)),s,gad(t,d)), pWeight_rp(rp)*pSlopeVarCost(t)*  vProduct    (y,p,s,t,d)] ;

eBalance_C_can  (y,pa(p),s,  d   )    .. sum[gad(g,d),  vProduct(y,p,s,g,d)]
                                      + vENS(y,p,s,d) =e=  pDemand(p,s) * pDemShare(d) * pCumDemIncr(y)
                                      + sum[la(d,df),  vFlow(y,p,s,d,df)] - sum[la(di,d), vFlow(y,p,s,di,d)]
                                      + sum(gad(h,d),vConsump(y,p,s,h,d)/pEffic(h))             ;


eStorage_C_can  (y,pa(p),gad(hf,d) )  .. vReserve (y,p-1,hf,d) + pIniReserve(hf)$p1(p) - vReserve(y,p,hf,d) + pInflows(p,hf)-vSpillage(y,p,hf,d)
                                      + sum((s), pDuration(p,s)*vConsump  (y,p,s,hf,d)/pProdFunct(hf))
                                      - sum((s), pDuration(p,s)*vProduct  (y,p,s,hf,d)/pProdFunct(hf))  =e= 0                      ;

*eStrtUpPer_C_can(y,p,s,t     )       .. vCommitt_can(y,p,s,t) =e=
*                                                    vCommitt_can(y,p,s-1,t  ) + vStartup_can(y,p,s,t  ) - vShutdown_can(y,p,s,t  ) ;
eStrtUpPer_C_can(y,pa(p),s,gad(t,d))$(card(p)>1).. vCommitt_can(y,p,s,t,d) =L= vCommitt_can(y,p-1,s,t,d) + vStartup_can(y,p,s,t,d) ;

E_eStrtUpPer_C_can_RP(y,rp,p,rrpp,s,gad(t,d)) $[pa(p) AND pFirstP_rp(rp,p   )AND pTransMatrix_rp(rp,rrpp) > pMinTransition]   ..

vCommitt_can  (y,p,s,t,d) =E= SUM[pp $[pFirstP_rp(rrpp,pp)], vCommitt_can     (y,pp+pNumPerMinus1_rp(rrpp),s,t,d)] ;

* Generation Investment
eMaxProd_C_can_0(y,pa(p),s,ged(g,d))$(t(g) or h(g))      ..  vProduct(y,p,s,g,d  )  =l=   pMaxProd(g)                                                    ;
eMaxProd_C_can_1(y,pa(p),s,gcd(g,d))     ..  vProduct(y,p,s,g,d  )   =l=   pMaxProd(g) *(vNewGen_can (y, g,d) )                             ;
eMaxProd_C_can_2(y,pa(p),s,gcd(g,d))$t(g)..  vProduct(y,p,s,g,d  )   =l=   pMaxProd(g) *(vCommitt_can(y,p,s,g,d)+(1 - vNewGen_can (y, g,d)))  ;
eMaxProd_C_can_3(y,pa(p),s,ged(g,d))$t(g)..  vProduct(y,p,s,g,d  )   =l=   pMaxProd(g) *(vCommitt_can(y,p,s,g,d))                           ;
eMinProd_C_can_1(y,pa(p),s,gcd(g,d))$t(g)..  vProduct(y,p,s,g ,d )   =g=   pMinProd(g) *(vCommitt_can(y,p,s,g,d)+(-1+ vNewGen_can (y, g,d)))  ;
eMinProd_C_can_2(y,pa(p),s,ged(g,d))$t(g)..  vProduct(y,p,s,g,d  )   =g=   pMinProd(g) *(vCommitt_can(y,p,s,g,d))                           ;
eGenInst_C_can  (y,        gcd(g,d))     .. vNewGen_can(y-1, g,d)   =l=   vNewGen_can  (y, g,d  )                                           ;
eMaxInst_C_can (y,     d) .. sum(g,  vNewGen_can (y, g,d)$gcd(g,d) ) =L=1;
eMinReserve     (y,pt(p),ged(h,d)   ) ..  vReserve(y,p,h,d)                       =g=   0  ;                                                              ; // b_Delta_lo (y,p,g    )
eMaxReserve     (y,pt(p),ged(h,d)   ) ..- vReserve(y,p,h,d)                       =g= -  pMaxReserve(h)  ;                                                              ; // b_Delta_up (y,p,g    )
eMinSpillage    (y,pa(p),ged(h,d)   ) ..  vSpillage(y,p,h,d)                      =g=    0          ;                                                             ; // b_mu_lo    (y,p,g    )
eMaxSpillage    (y,pa(p),ged(h,d)   ) ..-  vSpillage(y,p,h,d)                      =g= -  pMaxReserve(h) ;

*Storage Investment
eMaxCons_C_can_1(y,pt(p),s,gad(h,d))     .. vConsump(y,p,s,h,d)/pEffic(h)   =l=   pMaxCons(h)  *(vNewGen_can (y, h,d) )    ;


eRSRVH_RP1_C_CAN (y,ps(p),h,d)$[ORD(p) < CARD(p)  and gad(h,d) ]..
               vReserve(y,p,h,d)=E=
*               // Reserve value at p-pStorMovWindow (Moving Window)
               vReserve(y,p-pStorMovWindow,h,d)$[ORD(p) >= pStorMovWindow]
*               // Reserve value for first period
            +  pIniReserve(h)$p1(p)
*               // Sum of production/consumption during the moving window
            +  SUM[pp$[ORD(pp) >= ORD(p)+1-pStorMovWindow
                   AND ORD(pp) <= ORD(p)],
*               // Sum taking into acount the RP relation among periods
                 SUM [ IndexRP (pp,ppp) ,
*               //     Hydro production
*                    - vProduct(y,ppp,s,h)
                    - sum((s), pDuration(p,s)*vProduct  (y,ppp,s,h,d) /pProdFunct(h) )
*               //     Spillage
                   - vSpillage  (y,ppp,h,d)
*               //     charging/pumping consumption
*                    + pEta(h) * vConsumption(ppp,h)
                    + sum((s), pDuration(p,s)*vConsump  (y,ppp,s,h,d)/pProdFunct(h))
*               //     inflows
                    + pInflows(ppp,h)]
                   ] ;

eRSRVH_RP2_C_CAN (y,ps(p),h,d) $[ORD(p) = CARD(p) and gad(h,d) ]..
                vReserve(y,p,h,d) =E=
               // Reserve value at last period os set ps(p)
              vReserve(y,p-pLastStorMovW,h,d)
               // Sum of production/consumption during the moving window
            +  SUM[pp$[ORD(pp) >= ORD(p)+1-pLastStorMovW
                   AND ORD(pp) <= ORD(p)],
               // Sum taking into acount the RP relation among periods
               +  SUM[indexRP(pp,ppp),
               //     Hydro production
                    - sum((s), pDuration(p,s)*vProduct  (y,ppp,s,h,d)/pProdFunct(h))
               //     Spillage
                    - vSpillage  (y,ppp,h,d)
               //     charging/pumping consumption
                    + sum((s), pDuration(p,s)*vConsump  (y,ppp,s,h,d)/pProdFunct(h))
               //     inflows
                    + pInflows(ppp,h)]
                  ] ;


* ----------------- Relaxed    candidates   generations units -------------------------

eTotalFCost_R_can                    .. vTotalFCost =e=
                                        sum[(y,gcd(g,d)),(card(y)-ord(y)+1)*pGenInvCost (g)*[vNewGen_R_can(y, g,d) - vNewGen_R_can(y-1, g,d)]]+
                                        sum[(y,lc     ), (card(y)-ord(y)+1)*pFixedCost (lc)*[vNewLine(y,lc) - vNewLine(y-1,lc)]]      ;

eTotalVCost_R_can                    .. vTotalVCost  =e=
                                        sum[(y,rpp(rp,pa(p)),s,  d),  pWeight_rp(rp)* pENSCost        *  vENS          (y,p,s,  d)] +
*                                       sum[(y,pa(p),s    ),                pPNSCost        *  vPNS          (y,b,s    )] +
                                        sum[(y,rpp(rp,pa(p)),s,gad(t,d)  ),  pWeight_rp(rp)*pStartupCost (t)*  vStartup_R_can(y,p,s,t,d  )] +
                                        sum[(y,rpp(rp,pa(p)),s,gad(t,d)   ),  pWeight_rp(rp)*pInterVarCost(t)*  vCommitt_R_can(y,p,s,t,d  )] +
                                        sum[(y,rpp(rp,pa(p)),s,gad(t,d)  ),  pWeight_rp(rp)*pSlopeVarCost(t)*  vProduct      (y,p,s,t,d  )]                    ;

eMaxInst_R_can (y,    d    ) .. sum (g,  vNewGen_R_can(y, g,d)$gcd(g,d))=l=1;

*eStrtUpPer_R_can(y,p,s,t)$[card(s)>1].. vCommitt_R_can(y,p,s,t) =L= vCommitt_R_can(y,p,s-1,t  ) + vStartup_R_can(y,p,s,t  )            ;
eStrtUpPer_R_can(y,pa(p),s,gad(t,d) )$(card(p)>1).. vCommitt_R_can(y,p,s,t,d) =L= vCommitt_R_can(y,p-1,s,t,d  ) + vStartup_R_can(y,p,s,t,d  )            ;

E_ACOP_RP      (y,rp,p,rrpp,s,gad(t,d)    )
  $[pa(p) AND pFirstP_rp     (rp,p   )
          AND pTransMatrix_rp(rp,rrpp) > pMinTransition]..

  vCommitt_R_can(y,p,s,t,d) =E=
                    SUM[pp $[pFirstP_rp(rrpp,pp)],
                        vCommitt_R_can     (y,pp+pNumPerMinus1_rp(rrpp),s,t,d)] ;

* Generation Investment
*eMaxProd_R_can_0(y,pa(p),s,ged(g,d))     .. vProduct(y,b,s,g  )   =l=   pMaxProd(g)        //same as eMaxProd_R_can_0                     ;
eMaxProd_R_can_1(y,pa(p),s,gcd(wn,d))     .. vProduct(y,p,s,wn,d  )   =l=   pWindGen     (p,wn) *(vNewGen_R_can (y, wn,d) )                               ;
eMaxProd_R_can_2(y,pa(p),s,gcd(g,d))$t(g).. vProduct(y,p,s,g,d  )   =l=   pMaxProd(g) *(vCommitt_R_can(y,p,s,g,d)+(1 - vNewGen_R_can (y, g,d)))  ;
eMaxProd_R_can_3(y,pa(p),s,ged(g,d))$t(g).. vProduct(y,p,s,g,d  )   =l=   pMaxProd(g) *(vCommitt_R_can(y,p,s,g,d))                             ;

eMinProd_R_can_1(y,pa(p),s,gcd(g,d))$t(g).. vProduct(y,p,s,g,d  )   =g=   pMinProd(g) *(vCommitt_R_can(y,p,s,g,d)+(-1+ vNewGen_R_can (y, g,d)))  ;
eMinProd_R_can_2(y,pa(p),s,ged(g,d))$t(g).. vProduct(y,p,s,g,d  )   =g=   pMinProd(g) *(vCommitt_R_can(y,p,s,g,d))                             ;
eMinProd_R_can_3(y,pa(p),s,ged(g,d))$t(g).. vProduct(y,p,s,g,d  )   =g=   pMinProd(g)                        ;

eGenInst_R_can  (y,    gcd(g,d))         .. vNewGen_R_can(y-1, g,d) =l=   vNewGen_R_can  (y, g,d  )                                            ;
eMaxCons_R_can_1(y,ps(p),s,gcd(h,d))     .. vConsump(y,p,s,h,d)/pEffic(h)   =l=   pMaxCons(h)  *(vNewGen_R_can (y, h,d) )    ;


* ----------------- Simple    can   generations units --------------------------

eTotalVCost_S_can .. vTotalVCost =e=  sum[(y,rpp(rp,pa(p)),s,  d), pWeight_rp(rp) * pDuration(p,s)*pENSCost        *vENS    (y,p,s,  d)] +
*                                     sum[(y,rpp(rp,pa(p)),s    ),                pPNSCost        *vPNS    (y,p,s    )] +
                                      sum[(y,rpp(rp,pa(p)),s,gad(t,d)), pWeight_rp(rp) * pDuration(p,s)*pSlopeVarCost(t)*vProduct (y,p,s,t,d)] +
                                      sum[(y,rpp(rp,pa(p)),s,gad(t,d), tg(t,th)), pWeight_rp(rp) *pCO2price*(vProduct (y,p,s,t,d)* pEmissionRate (th))] ;

* -------------------- Constraints common to all models ------------------------

*Network Constraints
eFlowNetEx      (y,pa(p),s, le(di,df)) .. vFlow(y,p,s,  di,df)                   =e= [vTheta(y,p,s,di) - vTheta(y,p,s,df)] * pSbase / pX(di,df)  ;
eFlowInstlCap1  (y,pa(p),s, lc(di,df)) .. vFlow(y,p,s,  di,df) /     pTTC(di,df) =g=                                                                                  - vNewLine(y,di,df) ;
eFlowInstlCap2  (y,pa(p),s, lc(di,df)) .. vFlow(y,p,s,  di,df) /     pTTC(di,df) =l=                                                                                    vNewLine(y,di,df) ;
eFlowNetN1      (y,pa(p),s, lc(di,df)) .. vFlow(y,p,s,  di,df) / 1e3*pTTC(di,df) =g= [vTheta(y,p,s,di) - vTheta(y,p,s,df)] * pSbase / pX(di,df) / 1e3*pTTC(di,df) - 1 + vNewLine(y,di,df) ;
eFlowNetN2      (y,pa(p),s, lc(di,df)) .. vFlow(y,p,s,  di,df) / 1e3*pTTC(di,df) =l= [vTheta(y,p,s,di) - vTheta(y,p,s,df)] * pSbase / pX(di,df) / 1e3*pTTC(di,df) + 1 - vNewLine(y,di,df) ;
eCumLineInstall (y,         lc       ) .. vNewLine(y-1,lc)       =l=   vNewLine(y, lc) ;


*-------------------------------------------------------------------------------
*                                MODELS
*-------------------------------------------------------------------------------

* Specification of equations to be used in the model

model NEMEX_COMPLETE_CAN
/
eTotalTCost
eTotalFCost_C_CAN
eTotalVCost_C_CAN
eBalance_C_CAN
eStrtUpPer_C_CAN
eStorage_C_CAN
eMaxCons_C_CAN_1
eMaxProd_C_CAN_0
eMaxProd_C_CAN_1
eMaxProd_C_CAN_2
eMaxProd_C_CAN_3
eMinProd_C_CAN_1
eMinProd_C_CAN_2
eMinReserve
eMaxReserve
eMaxSpillage
eMinSpillage
eGenInst_C_CAN
eCumLineInstall
eMaxInst_C_can
eFlowNetEx
eFlowInstlCap1
eFlowInstlCap2
eFlowNetN1
eFlowNetN2
E_eStrtUpPer_C_can_RP
*E_ACOP_RP
eRSRVH_RP1_C_CAN
eRSRVH_RP2_C_CAN
 / ;

NEMEX_COMPLETE_CAN.SolPrint = 1 ; NEMEX_COMPLETE_CAN.HoldFixed = 1 ;


model NEMEX_RELAXED_CAN
/
eTotalTCost
eTotalFCost_R_can
eTotalVCost_R_can
eBalance_C_can
eStrtUpPer_R_can
eStorage_C_can
eMaxCons_R_CAN_1
eMaxProd_C_CAN_0
eMaxProd_R_CAN_1
eMaxProd_R_CAN_2
eMaxProd_R_CAN_3
eMinProd_R_CAN_1
eMinProd_R_CAN_2
eMinReserve
eMaxReserve
eMaxSpillage
eMinSpillage
eGenInst_R_CAN
eCumLineInstall
eMaxInst_R_can
eFlowNetEx
eFlowInstlCap1
eFlowInstlCap2
eFlowNetN1
eFlowNetN2
E_ACOP_RP
eRSRVH_RP1_C_CAN
eRSRVH_RP2_C_CAN
 / ;

NEMEX_RELAXED_CAN.SolPrint = 1 ; NEMEX_RELAXED_CAN.HoldFixed = 1 ;


model NEMEX_SIMPLE_CAN
/
NEMEX_RELAXED_can
- eMaxProd_R_can_2
- eMaxProd_R_can_3
- eMinProd_R_can_1
- eMinProd_R_can_2
+ eMinProd_R_can_3
- eStrtUpPer_R_can
- eTotalVCost_R_can
- E_ACOP_RP
+ eTotalVCost_S_can
 /
 ;

NEMEX_SIMPLE_CAN.SolPrint = 1 ; NEMEX_SIMPLE_CAN.HoldFixed = 1 ;



* read input data from Excel and include into the model

file TMP / %gams.user1%.txt /
$OnEcho  > %gams.user1%.txt
   r1=    indices
   o1=tmp_indices.txt
   r2=    param
   o2=tmp_param.txt
   r3=    demand
   o3=tmp_demand.txt
   r4=    oprres
   o4=tmp_oprres.txt
   r5=    demand_dur
   o5=tmp_demand_dur.txt
   r6=    thermalgen
   o6=tmp_thermalgen.txt
   r7=    storagegen
   o7=tmp_storagegen.txt
   r8=    inflows
   o8=tmp_inflows.txt
   r9=    HourlyDemand
   o9=tmp_HourlyDemand.txt
   r10=    network
   o10=tmp_network.txt
   r11=    renewgen
   o11=tmp_renewgen.txt
   r12=    ReprePeriods
   o12=tmp_repreperiods.txt
   r13=TransitionMatrix_rp
   o13=TransitionMatrix_rp
   r14=    Hindex_RP
   o14=    indexRP.txt
   r15=    Solar
   o15=tmp_solar.txt
   r16=    Wind
   o16=tmp_wind.txt
   r22=Weight
   o22=Weight

$OffEcho
* Mac OS X and Linux users must comment the following call and copy and paste the named ranges of the Excel interface into the txt files
$ifthen.OptSkipExcelInput '%OptSkipExcelInput%' == '0'
$call xls2gms m i="%gams.user1%.xlsm" @"%gams.user1%.txt"
$else.OptSkipExcelInput
$  log Excel input skipped
$endif.OptSkipExcelInput

sets
$include tmp_indices.txt
$include tmp_repreperiods.txt
;
$include tmp_param.txt
parameter
$INCLUDE Weight
;

table    pDemand(p,s)
$include tmp_HourlyDemand.txt
*table    pOperReserve(p,s)
*$include tmp_oprres.txt
table    pDuration(p,s)
$include tmp_demand_dur.txt
table    pThermalGen(g,*)
$include tmp_thermalgen.txt
table    pStorageGen(g,*)
$include tmp_storagegen.txt
table    pRenewGen(g,*)
$include tmp_renewgen.txt
table    tSolarGen(p,g)
$include tmp_solar.txt
table    pWindGen(p,g)
$include tmp_wind.txt
table    pNetwork(d,d,*)
$include tmp_network.txt
table    pInflow(p,g)
$include tmp_inflows.txt
TABLE    pTransMatrix_rp(rp,rrpp)
$INCLUDE TransitionMatrix_rp
TABLE    pTransMatrix_rp(rp,rrpp)
sets
$include indexRP.txt
;


* Delete the loaded ranges from memory
EXECUTE 'del tmp.txt tmp_indices tmp_param tmp_demand tmp_oprres tmp_demand_dur'  ;
EXECUTE 'del tmp.txt tmp_thermalgen tmp_storagegen tmp_inflows tmp_HourlyDemand' ;
EXECUTE 'del tmp.txt tmp_network tmp_renewgen tmp_repreperiods' ;



* Mac OS X and Linux users must comment the following execute
execute 'del tmp_"%gams.user1%".txt tmp_indices.txt tmp_param.txt tmp_demand.txt tmp_oprres.txt tmp_duration.txt tmp_thermalgen.txt tmp_storagegen.txt tmp_inflows.txt tmp_network.txt' ;
pa(p) $[SUM[rpp(rp,p),1]] = YES ;
t (g) $[pThermalGen(g,'MaxProd'   ) and pThermalGen(g,'FuelCost')] = yes ;
h (g) $[pStorageGen(g,'MaxProd'   )                              ] = yes ;
sl(g) $[tSolarGen  ('h0001',g)                                   ] = yes ;
wn(g) $[pWindGen   ('h0001',g)                                  ] = yes ;
hf(h) $[    pStorageGen (h,'Type'      )] = YES ; // activating "fast" storage technology
hs(h) $[NOT pStorageGen (h,'Type'      )] = YES ; // activating "slow" storage technology
w (g) $[pRenewGen  (g,'MaxProd'   )                              ] = yes ;

z            (y)  = yes ;
pOrder       (y)  = ord(y) ;
pCumDemIncr  (y)  = prod[z $[pOrder(z) <= ord(y)], 1+pDemIncr(z)] ;
p1(p)$(ord(p)=1)  = yes;
pn(p)$(ord(p)=card(p)) = yes;
n1(s)$(ord(s)=1)  = yes;
nn(s)$(ord(s)=24) = yes;
pMinTransition=0;

* Scaling parameters
pDemand     (p,s)   = pDemand     (p,s)              * 1e-3 ;
pENSCost            = pENSCost                       * 1e-3 ;
pPNSCost            = pPNSCost                       * 1e-3 ;
*Review this scalling
pGenInvCost  (t)    = pThermalGen(t,'InvCost'      )        ;
pGenInvCost  (wn)    = pRenewGen  (wn,'InvCost'      )        ;
pGenInvCost  (h)    = pStorageGen(h,'InvCost'      )        ;
pTTC      (di,df)   = pNetwork   (di,df,'TTC'      ) * 1e-3 ;
pEFOR        (t)    = pThermalGen(t,'EFOR'         )        ;
pSlopeVarCost(t)    = pThermalGen(t,'OMVarCost'    ) * 1e-3 +
*                      pThermalGen(t,'SlopeVarCost' ) *
                      1e-3 * pThermalGen(t,'FuelCost') ;
pInterVarCost(t)    = pThermalGen(t,'InterVarCost' ) * 1e-6 * pThermalGen(t,'FuelCost') ;
pStartupCost (t)    = pThermalGen(t,'StartupCost'  ) * 1e-6 * pThermalGen(t,'FuelCost')+eps ;
pFixedCost(di,df)   = pNetwork   (di,df,'FixedCost') * pNetwork(di,df,'FxChargeRate') ;
pX        (di,df)   = pNetwork   (di,df,'X'        )        ;
pSbase              = pSbase                         * 1e-3 ;
pMaxProd     (t)    = pThermalGen(t,'MaxProd'      ) * 1e-3 * [1-pEFOR(t)] ;
pMaxProd     (h)    = pStorageGen(h,'MaxProd'      ) * 1e-3 ;
*pMaxProd     (sl)   = tSolarGen  (       ,sl             ) * 1e-3 ;
pMaxProdSolar(p,sl) = tSolarGen  (p,sl             ) * 1e-3;
pWindGen     (p,wn)  = pWindGen  (p,wn             ) * 1e-3;
pMinProd     (t)    = pThermalGen(t,'MinProd'      ) * 1e-3 * [1-pEFOR(t)] ;
pMaxProd     (w)    = pRenewGen  (w,'MaxProd'      ) * 1e-3 * [1-pEFOR(w)] ;

pEmissionRate (th)  = pEmissionRate (th)             * 1e-3       ;
pCO2price           = pCO2price                           ;



* Storage Parameters
pMaxCons     (h)    = pStorageGen(h,'MaxCons'      ) * 1e-3 ;
pProdFunct   (h)    = pStorageGen(h,'ProdFunct'    ) * 1e+3 ;
pEffic       (h)    = pStorageGen(h,'Efficiency'   )        ;
pMaxReserve  (h)    = pStorageGen(h,'MaxReserve'   ) * 1e-3 ;
pMinReserve  (h)    = pStorageGen(h,'MinReserve'   ) * 1e-3 ;
pIniReserve  (h)    = pStorageGen(h,'IniReserve'   ) * 1e-3 ;
pFinReserve  (h)    = pStorageGen(h,'FinReserve'   ) * 1e-3 ;
*why by 3.6
pInflows    (p,h)    = pInflow     (p,h              ) * 1e-6 * 3.6*sum[(s), pDuration(p,s)] ;

pProdFunct(h) $[pProdFunct(h) = 0] = 1e3 ;
pEffic    (h) $[pEffic    (h) = 0] =   1 ;
*Sets activation
lc(   di,df ) $pFixedCost(di,df)  = yes ;
la(   di,df ) $pX        (di,df)  = yes ;
le(la(di,df)) $[not    lc(di,df)] = yes ;

gnd(g,d) $[not         ged(g,d)]  = yes;
gad(g,d) $[gcd(g,d) or ged(g,d)]  = yes;

* Scaling Transition Matrix for Representative Periods, Normalizing
pMaxTM_rp = SMAX[(rp,rrpp), pTransMatrix_rp(rp,rrpp)]           ;
pTransMatrix_rp  (rp,rrpp)= pTransMatrix_rp(rp,rrpp) / pMaxTM_rp;

* Representative periods information
pNumPer_rp(rp)                       = SUM[p $[rpp(rp,p)],1]           ;
pWeight_rp(rp) $[NOT pNumPer_rp(rp)] = 0 ;
pWeight_rp(rp) $[    pNumPer_rp(rp)] = pWeight_rp(rp) / pNumPer_rp(rp) ;
display pWeight_rp;

* Procedure to find the first period at each representative day
pFirstP_rp(rp,p) $[NOT rpp(rp,p)] = 0      ;
pFirstP_rp(rp,p) $[    rpp(rp,p)] = ORD(p) ;
pFirstP_rp(rp,p)                  = + pFirstP_rp(rp,p  )
                                    - pFirstP_rp(rp,p-1)$[ORD(p) > 1]
                                    + 1                 $[ORD(p) = 1];
pFirstP_rp(rp,p) $[NOT rpp(rp,p)] = 0 ;
pFirstP_rp(rp,p) $[    rpp(rp,p) AND pFirstP_rp(rp,p) = 1] = 0 ;
pFirstP_rp(rp,p) $[    rpp(rp,p) AND           ORD(p) = 1] = 1 ;

* Definition of parameter indicating the number of periods in the RP minus 1
pNumPerMinus1_rp(rp) = pNumPer_rp(rp) - 1 ;

* Activating set of periods to calculate storage level for "slow" units in RP-TM model
*FOR(pScalar = 1 to CARD(p) by pStorMovWindow,
*   ps(p) $[ORD(p) = pScalar] = yes;
*);
   ps(p) $(mod(ORD(p)-1,pStorMovWindow)=0) = yes;
   ps(p) $[ORD(p) = CARD(p)] = YES ; // also including the last period
   pt(p)$ [ps(p) or pa(p)]= yes;

* Parameter to indicate the last hour on ps(p) set
IF(Mod[CARD(p),pStorMovWindow]=0,
   pLastStorMovW = pStorMovWindow - 1 ;
ELSE
   pLastStorMovW = CARD(p) - Floor[CARD(p)/pStorMovWindow] * pStorMovWindow - 1 ;
);


display pFixedCost,lc,gcd,ged,gad, t,ps;
* Definition of upper and lower Bounds

vProduct.up (y,pa(p),s,sl,d) = pMaxProdSolar(p,sl);
vProduct.up (y,pa(p),s,wn,d) = pWindGen     (p,wn);

*vProduct_all.up(y,p,s,g,d )      = pMaxProd(g)    ;
*vProduct.up    (y,p,s,g   )      = pMaxProd(g)    ;
*vFlow.lo       (y,pa(p),s,la  )      = - pTTC(la)     ;
*vFlow.up       (y,pa(p),s,la  )      =   pTTC(la)     ;
*vReserve.up    (y,pa(p),h,d     )      = pMaxReserve(h) ;
*vReserve.lo    (y,pa(p),h,d     )      = pMinReserve(h) ;
*vReserve.fx    ('yr05',pa(p),h)$pn(p)= pIniReserve(h) ;
*vConsump_all.up(y,pa(p),s,h,d )      = pMaxCons(h)        ;
*vConsump.up    (y,pa(p),s,h,d )        = pMaxCons(h)        ;
*vNewGen.up (y, g,d)   = 3;


*-------------------------------------------------------------------------------
*                                MODEL SOLVES
*-------------------------------------------------------------------------------
vTheta.fx  (y,p,s,di)$[ord(di)=1] = 0 ;

* Scaling Results

* data output to xls file

IF(pModel = 1 and pSearch = 2 ,
*vCommitt_can.fx('yr01',p,'n01',t)$(sum(rp$pFirstP_rp('rp04',p),1))=0;
SOLVE NEMEX_COMPLETE_CAN using MIP minimizing vTotalTCost ;

pa(p) = NO  ;
pa(p) $[SUM[rpp(rp,p),1]] = YES ;
display pa;
pTotalCost                       =    sum[(y,rpp(rp,pa(p)),s,t,d), pWeight_rp(rp)*pSlopeVarCost(t)*  vProduct.l(y,p,s,t,d)]  ;
*pProduct       (y,pa(p),th)   =    sum((s,d,g), vProduct.l  (y,p,s,g,d)$tg(g,th))            ;
pProduct       (y,pa(p),g )   =    sum((s,d), vProduct.l  (y,p,s,g,d))            ;
pInstalCapT_can(y,    g,d  )       =    vNewGen_can.l   (y,    g,d)        ;
pCommit_can    (y,pa(p),s,t,d  )   =    vCommitt_can.l  (y,p,s,t,d)   +eps         ;
pFlow          (y,pa(p),s, la)   =  [ vFlow.l     (y,p,s, la)]*1e3    ;
pTheta         (y,pa(p),s,  d)   =  [ vTheta.l    (y,p,s,  d)]        ;
pReserve       (y,ps(p),gad(h,d) )   =    vReserve.l  (y,p,h,d);
pInstalLine    (y,         lc)   =   vNewLine.l  (y,lc);
pStartup       (y,pa(p),s,t,d  )   =   vStartup_can.l   (y,p,s,t,d  )  +eps        ;
pENS           (y,  s,     d)    =    sum(p,vENS.l   (y,p,s,  d))                  ;

OF_Cost    ('Obj Func  Model      [1000 M€]') = pTotalCost + EPS ;
GenCPUTime ('CPU Time  Model generation [s]') = NEMEX_COMPLETE_CAN.resGen  ;
SolCPUTime ('CPU Time  Model solution   [s]') = NEMEX_COMPLETE_CAN.resUsd  ;
NumVar     ('Number of variables           ') = NEMEX_COMPLETE_CAN.numVar  ;
NumDVar    ('Number of discrete variables  ') = NEMEX_COMPLETE_CAN.numDVar ;
NumEqu     ('Number of equations           ') = NEMEX_COMPLETE_CAN.numEqu  ;
NumNZ      ('Number of nonzero elements    ') = NEMEX_COMPLETE_CAN.numNZ   ;
BestSol    ('Best possible solution for MIP') = NEMEX_COMPLETE_CAN.objest  ;

put TMP putclose 'par=pInstalCapT_can rdim=1 rng=GenInv!a1' / 'par=pFlow rdim=4 rng=Flow!a1' / 'par=pTheta rdim=3 rng=Angle!a1'
/'par=pCommit_can rdim=3 rng=UC!a1' / 'par=pProduct rdim=2 rng=Output!a1' / 'par=pInstalLine rdim=1 rng=LineInv!a1' /
 'par=pReserve rdim=2 rng=WtrReserve!a1' / 'par=pStartup rdim=3 rng=StartUp!a1' / 'par=pENS rdim=3 rng=ENS!a1' /
 'par=OF_Cost rdim=1 rng=Cost!a1' / ' par=GenCPUTime rdim=1 rng=Cost!a2'  / ' par=SolCPUTime rdim=1 rng=Cost!a3'  /
 'par=NumVar rdim=1 rng=Cost!a4' / ' par=NumDVar rdim=1 rng=Cost!a5'  / ' par=NumEqu rdim=1 rng=Cost!a6'/ ' par=BestSol rdim=1 rng=Cost!a7'
execute_unload   '%gams.user1%.gdx' pInstalCapT_can pFlow pTheta pCommit_can pProduct pInstalLine pReserve pStartup  pENS OF_Cost GenCPUTime SolCPUTime NumVar NumDVar NumEqu NumNZ BestSol
execute          'gdxxrw "%gams.user1%".gdx SQ=n EpsOut=0 O="%gams.user1%".xlsx @"%gams.user1%".txt'
execute          'del    "%gams.user1%".gdx '
);


IF(pModel = 2 and pSearch = 2 ,
vCommitt_can.fx('yr01',p,'n01',t,d)$(sum(rp$pFirstP_rp('rp01',p),1))=0;
SOLVE NEMEX_RELAXED_CAN using MIP minimizing vTotalTCost ;
pa(p) = NO  ;
pa(p) $[SUM[rpp(rp,p),1]] = YES ;
pTotalCost                       =    sum[(y,rpp(rp,pa(p)),s,t,d), pWeight_rp(rp)*pSlopeVarCost(t)*  vProduct.l(y,p,s,t,d)]  ;
*pProduct       (y,pa(p),th)   =    sum((s,d,g), vProduct.l  (y,p,s,g,d)$tg(g,th))      ;
pProduct       (y,pa(p),g )   =    sum((s,d), vProduct.l  (y,p,s,g,d))            ;
pInstalCapT_can(y,        g,d  )   =    vNewGen_R_can.l   (y,    g,d)                  ;
pCommit_can    (y,pa(p),s,t,d  )   =    vCommitt_R_can.l  (y,p,s,t,d)                  ;
pFlow          (y,pa(p),s, la)   =  [ vFlow.l     (y,p,s, la)]*1e3                 ;
pTheta         (y,pa(p),s,  d)   =  [ vTheta.l    (y,p,s,  d)]                     ;
pReserve       (y,ps(p),gad(h,d)    )   =    vReserve.l  (y,p,h,d)                          ;
pInstalLine    (y,lc         )   =   vNewLine.l  (y,lc)                            ;
pStartup       (y,pa(p),s,t,d  )   =   vStartup_R_can.l   (y,p,s,t,d  )                ;
pENS           (y,  s,      d)   =   sum(p,vENS.l(y,p,s,    d))                    ;

OF_Cost    ('Obj Func  Model      [1000 M€]') = pTotalCost + EPS ;
GenCPUTime ('CPU Time  Model generation [s]') = NEMEX_RELAXED_CAN.resGen  ;
SolCPUTime ('CPU Time  Model solution   [s]') = NEMEX_RELAXED_CAN.resUsd  ;
NumVar     ('Number of variables           ') = NEMEX_RELAXED_CAN.numVar  ;
NumDVar    ('Number of discrete variables  ') = NEMEX_RELAXED_CAN.numDVar ;
NumEqu     ('Number of equations           ') = NEMEX_RELAXED_CAN.numEqu  ;
NumNZ      ('Number of nonzero elements    ') = NEMEX_RELAXED_CAN.numNZ   ;
BestSol    ('Best possible solution for MIP') = NEMEX_RELAXED_CAN.objest  ;

put TMP putclose 'par=pInstalCapT_can rdim=1 rng=GenInv!a1' / 'par=pFlow rdim=4 rng=Flow!a1' / 'par=pTheta rdim=3 rng=Angle!a1'
/'par=pCommit_can rdim=3 rng=UC!a1' / 'par=pProduct rdim=2 rng=Output!a1' / 'par=pInstalLine rdim=1 rng=LineInv!a1' /
 'par=pReserve rdim=2 rng=WtrReserve!a1' / 'par=pStartup rdim=3 rng=StartUp!a1' / 'par=pENS rdim=3 rng=ENS!a1' /
'par=OF_Cost rdim=1 rng=Cost!a1' / ' par=GenCPUTime rdim=1 rng=Cost!a2'  / ' par=SolCPUTime rdim=1 rng=Cost!a3'  /
 'par=NumVar rdim=1 rng=Cost!a4' / ' par=NumDVar rdim=1 rng=Cost!a5'  / ' par=NumEqu rdim=1 rng=Cost!a6'/ ' par=BestSol rdim=1 rng=Cost!a7'
execute_unload   '%gams.user1%.gdx' pInstalCapT_can pFlow pTheta pCommit_can pProduct pInstalLine pReserve pStartup pENS OF_Cost GenCPUTime SolCPUTime NumVar NumDVar NumEqu NumNZ BestSol
execute          'gdxxrw "%gams.user1%".gdx SQ=n EpsOut=0 O="%gams.user1%".xlsx @"%gams.user1%".txt'
execute          'del    "%gams.user1%".gdx '

);



IF(pModel = 3 and pSearch = 2 ,
vProduct_all.up(y,p,s,ged(g,d))    = pMaxProd(g)    ;
vCommitt_can.fx('yr01',p,'n01',t,d)$(sum(rp$pFirstP_rp('rp01',p),1))=0;
SOLVE NEMEX_SIMPLE_CAN using MIP minimizing vTotalTCost ;
pa(p) = NO  ;
pa(p) $[SUM[rpp(rp,p),1]] = YES ;
pTotalCost                       =    sum[(y,rpp(rp,pa(p)),s,t,d), pWeight_rp(rp)*pSlopeVarCost(t)*  vProduct.l(y,p,s,t,d)]  ;
pProductT      (y,pa(p),th)   =    sum((s,d,g), vProduct.l  (y,p,s,g,d)$tg(g,th))   ;
pProduct       (y,pa(p),g )   =    sum((s,d), vProduct.l  (y,p,s,g,d))            ;
pAnnualEnergy (y  ,     th  )  =    sum(pa(p), pProductT      (y,p,th) )      ;
pInstalCapT_can(y,    g,d      )   =    vNewGen_R_can.l   (y,    g,d)                    ;
*pCommit_can    (y,pa(p),s,t  )   =    vCommitt_can.l  (y,p,s,t)                    ;
pFlow          (y,pa(p),s, la)   =  [ vFlow.l     (y,p,s, la)]*1e3                 ;
pTheta         (y,pa(p),s,  d)   =  [ vTheta.l    (y,p,s,  d)]                     ;
pReserve       (y,ps(p),  gad(h,d))   =    vReserve.l  (y,p,h,d) + EPS                    ;
pInstalLine    (y,         lc)   =    vNewLine.l  (y,lc)                           ;
pStartup       (y,pa(p),s,gad(t,d))   =    vStartup_can.l   (y,p,s,t,d  )                 ;
pENS           (y,  s,      d)   =    sum(p,vENS.l(y,p,s,    d))                   ;

OF_Cost    ('Obj Func  Model      [1000 M€]') = pTotalCost + EPS ;
GenCPUTime ('CPU Time  Model generation [s]') = NEMEX_SIMPLE_CAN.resGen  ;
SolCPUTime ('CPU Time  Model solution   [s]') = NEMEX_SIMPLE_CAN.resUsd  ;
NumVar     ('Number of variables           ') = NEMEX_SIMPLE_CAN.numVar  ;
NumDVar    ('Number of discrete variables  ') = NEMEX_SIMPLE_CAN.numDVar ;
NumEqu     ('Number of equations           ') = NEMEX_SIMPLE_CAN.numEqu  ;
NumNZ      ('Number of nonzero elements    ') = NEMEX_SIMPLE_CAN.numNZ   ;
BestSol    ('Best possible solution for MIP') = NEMEX_SIMPLE_CAN.objest  ;

put TMP putclose 'par=pInstalCapT_can rdim=1 rng=GenInv!a1' / 'par=pFlow rdim=4 rng=Flow!a1' / 'par=pTheta rdim=3 rng=Angle!a1'
/'par=pCommit_can rdim=3 rng=UC!a1' / 'par=pProduct rdim=2 rng=Output!a1' / 'par=pInstalLine rdim=1 rng=LineInv!a1' /
'par=pReserve rdim=2 rng=WtrReserve!a1' / 'par=pStartup rdim=3 rng=StartUp!a1' /'par=pENS rdim=3 rng=ENS!a1' /
'par=OF_Cost rdim=1 rng=Cost!a1' / ' par=GenCPUTime rdim=1 rng=Cost!a2'  / ' par=SolCPUTime rdim=1 rng=Cost!a3'  /
 'par=NumVar rdim=1 rng=Cost!a4' / ' par=NumDVar rdim=1 rng=Cost!a5'  / ' par=NumEqu rdim=1 rng=Cost!a6'/ ' par=BestSol rdim=1 rng=Cost!a7'/
 'par=pProductT rdim=2 rng=OutputT!a1'/     'par=pAnnualEnergy rdim=2 rng=Energy!a1'/
execute_unload   '%gams.user1%.gdx' pInstalCapT_can pFlow pTheta pCommit_can pProduct pInstalLine pReserve pStartup pENS OF_Cost GenCPUTime SolCPUTime NumVar NumDVar NumEqu NumNZ BestSol pAnnualEnergy  pProductT
execute          'gdxxrw "%gams.user1%".gdx SQ=n EpsOut=0 O="%gams.user1%".xlsx @"%gams.user1%".txt'
execute          'del    "%gams.user1%".gdx '
);


*$ifthen.OptSkipExcelOutput '%OptSkipExcelOutput%' == 0
*$else.OptSkipExcelOutput
*$  log Excel output skipped
*$endif.OptSkipExcelOutput

$onlisting
