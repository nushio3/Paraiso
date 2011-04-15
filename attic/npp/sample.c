       const int idx0 = jindex[j+0];                                                                   
       const int idx1 = jindex[j+1];                                                                   
       const v2df xj = {particlej[idx0][0], particlej[idx1][0]};                                       
       const v2df yj = {particlej[idx0][1], particlej[idx1][1]};                                       
       const v2df zj = {particlej[idx0][2], particlej[idx1][2]};                                       
       const v2df cj = {particlej[idx0][3], particlej[idx1][3]};                                       
       const v2df lj0 = {lj[j+0][0], lj[j+1][0]};                                                      
       const v2df lj1 = {lj[j+0][1], lj[j+1][1]};                                                      
       const v2df lj2 = {lj[j+0][2], lj[j+1][2]};                                                      
       const v2df lj3 = {lj[j+0][3], lj[j+1][3]};                                                      
                                                                                                       
       const v2df dx = xi - xj;                                                                        
       const v2df dy = yi - yj;                                                                        
       const v2df dz = zi - zj;                                                                        
       const v2df cij = ci * cj;                                                                       
       const v2df r2_ = dx*dx + dy*dy + dz*dz;                                                         
       const v2df r2 = __builtin_ia32_minpd(r2_, rc2);                                                 
       const v2df ri1 = v2df_rsqrt_36(r2);                                                             
       const v2df r1  = ri1 * r2;                                                                      
       const v2df r3 = r1 * r2;                                                                        
       const v2df ri2 = ri1 * ri1; // temporal                                                         
       const v2df ri3 = ri2 * ri1;                                                                     
       const v2df ri4 = ri2 * ri2; // temporal                                                         
       const v2df ri6 = ri3 * ri3;                                                                     
       const v2df ri8 = ri4 * ri4;                                                                     
       const v2df ri12 = ri6 * ri6;                                                                    
       const v2df ri14 = ri8 * ri6;                                                                    
       const v2df de = lj0*(ri12+(r3*(r12co1-r12co2*r1)-r12co0))                                       
                     - lj1*(ri6 +(r3*(r6co1 -r6co2 *r1)-r6co0 ))                                       
                     + cij*(ri1 +(r3*(r1co1 -r1co2 *r1)-r1co0 ));                                      
       const v2df dp = lj2*(ri14-r1*(r14co1-r14co2*r1))                                                
                     - lj3*(ri8 -r1*(r8co1 -r8co2 *r1))                                                
                     + cij*(ri3 -r1*(r3co1 -r3co2 *r1));                                               
       en += de;                                                                                       
       fx += dx*dp;                                                                                    
       fy += dy*dp;                                                                                    
       fz += dz*dp;
