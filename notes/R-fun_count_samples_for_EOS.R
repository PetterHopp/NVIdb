Etter gårsdagen ut-testing viser deg seg at den mest farbare vei er slik:
  
  Noen av rapportene teller  antall prøvenumre, og de er nå som før, dvs det er ikke gjort noe forsøk på å endre dette. 

Noen rapporter har hatt inkonsekvent og ufullstendig telling.  Den eneste farbare fellesløsning her ser ut til å være denne:
  
  WHEN p.provetypekode = '01' THEN  p.antall_i_prove   -- single sample (n, NULL)                
WHEN p.provetypekode = '02' THEN   p.antall_i_prove  -- dual sample  (n, NULL)            
WHEN p.antall_i_samleprove IS NULL THEN  p.antall_i_prove  --sample collection , (n> 0, NULL)              
WHEN p.antall_i_prove > 1 THEN  p.antall_i_prove  -- sample collection (n> 1 ,m > 0). Do not multiply              
ELSE     p.antall_i_samleprove -- sample collection (1,m)
END
Det betyr at der hvor vi har antall_i_prove > 1  OG samleprøve, skal det ikke multipliseres opp, idet det gir en granularitet på prøvetallet som i de fleste tilfeller ikke er reell.

Etter å ha testet dette i dag morges, viser det seg at alt da er som før eller svakt bedre og at problemsakene forsvinner.

Det er nå ikke lengre nødvendig å føre samleprøvetallet i begge felt.  

