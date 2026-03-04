functions {
  // ---- reduce_sum partial likelihood functions for within-chain parallelization ----

  // Vehicle index effort counts
  real partial_sum_V(array[] int V_I_slice,
                     int start, int end,
                     array[] int day_V, array[] int section_V, array[] int countnum_V,
                     array[,] matrix lambda_E_S_I,
                     matrix p_TI, vector R_V, vector b) {
    real lp = 0;
    for (i in start:end) {
      int n = i - start + 1;
      lp += poisson_lpmf(V_I_slice[n] |
        (lambda_E_S_I[section_V[i], countnum_V[i]][day_V[i], 1] * p_TI[1, section_V[i]] * R_V[1] +
         lambda_E_S_I[section_V[i], countnum_V[i]][day_V[i], 2] * p_TI[2, section_V[i]] * R_V[2]) * b[1]);
    }
    return lp;
  }

  // Trailer index effort counts
  real partial_sum_T(array[] int T_I_slice,
                     int start, int end,
                     array[] int day_T, array[] int section_T, array[] int countnum_T,
                     array[,] matrix lambda_E_S_I,
                     matrix p_TI, vector R_T, vector b) {
    real lp = 0;
    for (i in start:end) {
      int n = i - start + 1;
      lp += poisson_lpmf(T_I_slice[n] |
        (lambda_E_S_I[section_T[i], countnum_T[i]][day_T[i], 1] * p_TI[1, section_T[i]] * R_T[1] +
         lambda_E_S_I[section_T[i], countnum_T[i]][day_T[i], 2] * p_TI[2, section_T[i]] * R_T[2]) * b[2]);
    }
    return lp;
  }

  // Angler index effort counts
  real partial_sum_A(array[] int A_I_slice,
                     int start, int end,
                     array[] int day_A, array[] int gear_A, array[] int section_A, array[] int countnum_A,
                     array[,] matrix lambda_E_S_I,
                     matrix p_TI, matrix p_I) {
    real lp = 0;
    for (i in start:end) {
      int n = i - start + 1;
      lp += poisson_lpmf(A_I_slice[n] |
        lambda_E_S_I[section_A[i], countnum_A[i]][day_A[i], gear_A[i]] * p_TI[gear_A[i], section_A[i]] * p_I[gear_A[i], section_A[i]]);
    }
    return lp;
  }

  // Census (tie-in) effort counts
  real partial_sum_E(array[] int E_s_slice,
                     int start, int end,
                     array[] int day_E, array[] int gear_E, array[] int section_E, array[] int countnum_E,
                     array[,] matrix lambda_E_S_I,
                     matrix p_TI) {
    real lp = 0;
    for (i in start:end) {
      int n = i - start + 1;
      lp += poisson_lpmf(E_s_slice[n] |
        lambda_E_S_I[section_E[i], countnum_E[i]][day_E[i], gear_E[i]] * p_TI[gear_E[i], section_E[i]]);
    }
    return lp;
  }

  // CPUE interview data
  real partial_sum_IntC(array[] int c_slice,
                        int start, int end,
                        array[] int day_IntC, array[] int gear_IntC, array[] int section_IntC,
                        vector h,
                        array[] matrix lambda_C_S,
                        real r_C) {
    real lp = 0;
    for (i in start:end) {
      int n = i - start + 1;
      lp += neg_binomial_2_lpmf(c_slice[n] |
        lambda_C_S[section_IntC[i]][day_IntC[i], gear_IntC[i]] * h[i], r_C);
    }
    return lp;
  }

  // Angler expansion interview data (vehicles + trailers)
  real partial_sum_IntA(array[] int V_A_slice,
                        int start, int end,
                        array[] int T_A, array[] int A_A,
                        array[] int gear_IntA,
                        vector R_V, vector R_T) {
    real lp = 0;
    for (i in start:end) {
      int n = i - start + 1;
      lp += binomial_lpmf(V_A_slice[n] | A_A[i], R_V[gear_IntA[i]]);
      lp += binomial_lpmf(T_A[i] | A_A[i], R_T[gear_IntA[i]]);
    }
    return lp;
  }
}
data{
    //Day attributes
	int<lower=0> D; //number of fishing days (sampling frame)
	int<lower=0> G; //number of unique gear/angler types 
	int<lower=0> S; //number of river sections
	int<lower=0> H; //max number of angler effort counts within a sample day across entire sampling frame (max countnum)
	int<lower=0> P_n; //number of periods (number of states for our state variable); P_n can equal D or some other interval (e.g., weekly)
	vector<lower=0,upper=1>[D] w; //index denoting daytype, where 0=weekday, 1= weekend/holiday.  
	array[D] int<lower=0> period; //index denoting period    
	vector<lower=0>[D] L; // total amount of available fishing hours per day (e.g., day length - sunrise to sunset)
	matrix<lower=0>[D,S] O; //index denoting fishery status, where 1=open, 0 = closed 
	//Vehicle index effort counts
	int<lower=0> V_n; //total number of individual vehicle index effort counts
	array[V_n] int<lower=0> day_V; //index denoting the "day" for an individual vehicle index effort count
	array[V_n] int<lower=0> section_V; //index denoting the "section" for an individual vehicle index effort count
	array[V_n] int<lower=0> countnum_V; //index denoting the "count number" for an individual vehicle index effort count
	array[V_n] int<lower=0> V_I; //number of vehicles enumerated during an individual index effort survey
	//Trailer index effort counts 
	int<lower=0> T_n; //total number of boat trailer index effort counts
	array[T_n] int<lower=0> day_T; //index denoting the "day" for an individual boat trailer index effort count
	array[T_n] int<lower=0> section_T; //index denoting the "section" for an individual boat trailer index effort count
	array[T_n] int<lower=0> countnum_T; //index denoting the "count number" for an individual boat trailer index effort count
	array[T_n] int<lower=0> T_I; //number of boat trailers enumerated during an individual index effort survey
	//Angler index effort counts
	int<lower=0> A_n; //total number of angler index effort counts             
	array[A_n] int<lower=0> day_A; //index denoting the "day" for an individual angler index effort count     
	array[A_n] int<lower=0> gear_A; //index denoting the "gear/angler type" for an individual angler index effort count (e.g., 1=bank and 2=boat)     
	array[A_n] int<lower=0> section_A; //index denoting the "section" for an individual angler index effort count  
	array[A_n] int<lower=0> countnum_A; //index denoting the "count number" for an individual angler index effort count
	array[A_n] int<lower=0> A_I; //number of anglers enumerated during an individual index effort survey         
	//Census (tie-in) effort counts
	int<lower=0> E_n; //total number of angler census effort counts 
	array[E_n] int<lower=0> day_E; //index denoting the "day" for an individual angler census effort count
	array[E_n] int<lower=0> gear_E; //index denoting the "gear/angler type" for an individual angler census effort count (e.g., 1=bank and 2=boat) 
	array[E_n] int<lower=0> section_E; //index denoting the "section" for an individual angler census effort count  
	array[E_n] int<lower=0> countnum_E; //index denoting the "count number" for an individual angler census effort count
	array[E_n] int<lower=0> E_s; //number of anglers enumerated during an individual census effort survey
	//Proportion tie-in expansion
	matrix<lower=0,upper=1>[G,S]p_TI; //proportion of section covered by tie in counts (serves as an expansion if p_TI != 1)
	//interview data - CPUE
	int<lower=0> IntC; //total number of angler interviews conducted across all surveys dates where CPUE data (c & h) were collected                                       	
	array[IntC] int<lower=0> day_IntC; //index denoting the "day" for an individual angler interview                       	
	array[IntC] int<lower=0> gear_IntC; //index denoting the "gear/angler type" for an individual angler interview (e.g., 1=bank and 2=boat)      						
	array[IntC] int<lower=0> section_IntC; //index denoting the "section" for an individual angler interview    						
	array[IntC] int<lower=0> c; // total number of fish caught by an angler (group) collected from an individual angler interview           						
	vector<lower=0>[IntC] h; // total number of hours fish by an angler (group) collected from an individual angler interview        						
	//interview data - angler expansion
	int<lower=0> IntA; //total number of angler interviews conducted across all surveys dates where angler expansion data (V_A, T_A, A_A) were collected                                         	
	array[IntA] int<lower=0> day_IntA; //index denoting the "day" for an individual angler interview                       	
	array[IntA] int<lower=0> gear_IntA; //index denoting the "gear/angler type" for an individual angler interview (e.g., 1=bank and 2=boat)      						
	array[IntA] int<lower=0> section_IntA; //index denoting the "section" for an individual angler interview    
	array[IntA] int<lower=0> V_A; // total number of vehicles by an individual angler/group brought to the fishery on a given survey date		
	array[IntA] int<lower=0> T_A; // total number of boat trailers by an individual angler/group brought to the fishery on a given survey date			
	array[IntA] int<lower=0> A_A; // total number of anglers in the group interviewed 
	//hyper and hyperhyper parameters
	real value_cauchyDF_sigma_eps_C; //the hyperhyper scale (degrees of freedom) parameter in the hyperprior distribution sigma_eps_C 
	real value_cauchyDF_sigma_eps_E; //the hyperhyper scale (degrees of freedom) parameter in the hyperprior distribution sigma_eps_E
	real value_cauchyDF_sigma_r_E; //the hyperhyper scale (degrees of freedom) parameter in the hyperprior distribution sigma_r_E
	real value_normal_sigma_B1; //the SD hyperparameter in the prior distribution B1
	real value_cauchyDF_sigma_r_C; //the hyper scale (degrees of freedom) parameter in the prior distribution sigma_r_C
	real value_betashape_phi_C_scaled; //the rate (alpha) and shape (beta) hyperparameters in phi_C_scaled 
	real value_betashape_phi_E_scaled; //the rate (alpha) and shape (beta) hyperparameters in phi_E_scaled 
	real value_normal_sigma_omega_C_0; // the SD hyperparameter in the prior distribution omega_C_0
	real value_normal_sigma_omega_E_0; // the SD hyperparameter in the prior distribution omega_E_0
	real value_lognormal_sigma_b; //the SD hyperparameter in the prior distribution b
	real value_normal_mu_mu_C; //the mean hyperparameter in the prior distribution mu_C
	real value_normal_sigma_mu_C; //the SD hyperparameter in the prior distribution mu_C
	real value_normal_mu_mu_E; //the mean hyperparameter in the prior distribution mu_E
	real value_normal_sigma_mu_E; //the SD hyperparameter in the prior distribution mu_E
  real value_cauchyDF_sigma_mu_C; //the hyperhyper SD parameter in the hyperprior distribution sigma_mu_C
	real value_cauchyDF_sigma_mu_E; //the hyperhyper SD parameter in the hyperprior distribution sigma_mu_E
	//Threading
	int<lower=1> grainsize; //reduce_sum grainsize (1 = auto-tune)
}
transformed data{

}
parameters{
	//Effort
	real B1; //fixed effect accounting for the effect of day type on effort   
	real<lower=0> sigma_eps_E; //effort process error standard deviation 
	cholesky_factor_corr[G*S] Lcorr_E; //effort process error correlations
	real<lower=0> sigma_r_E; //prior on r_E
	real<lower=0,upper=1> phi_E_scaled; //prior on a transformation of phi_E								    			
	matrix[P_n-1,G*S] eps_E; //effort process errors  
	matrix[G,S] omega_E_0; //effort residual for initial time step (p)
	vector<lower=0,upper=1>[G] R_V; //true angler vehicles per angler
	vector<lower=0,upper=1>[G] R_T; //true angler trailers per angler
	vector<lower=0>[G] b; //bias in angler vehicles per angler from road counts of cars
	array[S,H] matrix<lower=0>[D,G] eps_E_H; //gamma random variate accounting for overdispersion in the census effort counts due to within-day variability in angler pressure
	matrix<lower=0,upper=1>[G,S] p_I; //fixed proportion of angler effort observed in an index area
	array[G] real mu_mu_E; //hyper-prior on mean of mu_E //TB 5/3/2019
	real<lower=0>sigma_mu_E; //hyper-prior on SD of mu_E //TB 5/3/2019   
	matrix[G,S] eps_mu_E;
  //Catch rates
	real<lower=0> sigma_eps_C; //catch rate (CPUE) process error standard deviation
	cholesky_factor_corr[G*S] Lcorr_C; //CPUE process error correlations     
	real<lower=0,upper=1> phi_C_scaled; //Prior on a transformation of phi_C									    			
	real<lower=0> sigma_r_C; //Prior on r_C
	matrix[P_n-1,G*S] eps_C; //CPUE process errors 
	matrix[G,S] omega_C_0; //CPUE residual for initial time step (p)   
  array[G] real mu_mu_C ; //hyper-prior on mean of mu_C //TB 5/3/2019
	real<lower=0>sigma_mu_C; //hyper-prior on SD of mu_C
	matrix[G,S] eps_mu_C;
                        							
}
transformed parameters{
	//effort
	matrix[G,S] mu_E; //season-long effort intercept  
	real<lower=-1,upper=1> phi_E; //auto-regressive (AR), mean-reverting lag-1 coefficient for effort 
	matrix[P_n,G*S] omega_E; //residual in effort 
	array[S] matrix<lower=0>[D,G] lambda_E_S; //mean daily effort
	array[S,H] matrix<lower=0>[D,G] lambda_E_S_I; //mean hourly effort
	real<lower=0> r_E; //over-dispersion parameter accounting for within day variability in effort
	//Catch rates
	matrix[G,S] mu_C; //season-long catch rate intercept 
	real<lower=-1,upper=1> phi_C; //auto-regressive (AR), mean-reverting lag-1 coefficient for CPUE 
	real<lower=0> r_C; //over-dispersion parameter accounting for among angler (group) variability in CPUE
	matrix[P_n,G*S] omega_C; //Residual in CPUE
	array[S] matrix<lower=0>[D,G] lambda_C_S; //mean daily CPUE

	r_E = 1 / square(sigma_r_E);
	r_C = 1 / square(sigma_r_C);
	phi_C = (phi_C_scaled * 2)-1;
	phi_E = (phi_E_scaled * 2)-1;
	omega_C[1,] = to_row_vector(omega_C_0);
	omega_E[1,] = to_row_vector(omega_E_0);
	for(p in 2:P_n){
		omega_C[p,] = to_row_vector(phi_C * to_vector(omega_C[p-1,]) + diag_pre_multiply(rep_vector(sigma_eps_C,G*S),Lcorr_C) * to_vector(eps_C[p-1,])); 
		omega_E[p,] = to_row_vector(phi_E * to_vector(omega_E[p-1,]) + diag_pre_multiply(rep_vector(sigma_eps_E,G*S),Lcorr_E) * to_vector(eps_E[p-1,])); 
	}
	for(g in 1:G){
	  for(s in 1:S){
	  	mu_C[g,s] = mu_mu_C[g] + eps_mu_C[g,s] * sigma_mu_C;
		  mu_E[g,s] = mu_mu_E[g] + eps_mu_E[g,s] * sigma_mu_E;
	  }
		for(d in 1:D){       
			for(s in 1:S){	
				lambda_C_S[s][d,g] = exp(mu_C[g,s] + to_matrix(omega_C[period[d],],G,S)[g,s]) * O[d,s];
				lambda_E_S[s][d,g] = exp(mu_E[g,s] + to_matrix(omega_E[period[d],],G,S)[g,s] + B1 * w[d])* O[d,s];
				for(i in 1:H){
					lambda_E_S_I[s,i][d,g] = lambda_E_S[s][d,g] * eps_E_H[s,i][d,g];									
				}
			}
		}
	}	
}
model{
	//Hyperpriors (effort hyperparameters)
	sigma_eps_E ~ cauchy(0,value_cauchyDF_sigma_eps_E); 
	Lcorr_E ~ lkj_corr_cholesky(1);                             						
  phi_E_scaled ~ beta(value_betashape_phi_E_scaled,value_betashape_phi_E_scaled);
	sigma_r_E ~ cauchy(0,value_cauchyDF_sigma_r_E); 
	B1 ~ normal(0,value_normal_sigma_B1);
	//Hyperpriors (CPUE hyperparameters)
	sigma_eps_C ~ cauchy(0,value_cauchyDF_sigma_eps_C); 
  Lcorr_C ~ lkj_corr_cholesky(1);                                						
  phi_C_scaled ~ beta(value_betashape_phi_C_scaled,value_betashape_phi_C_scaled);
	sigma_r_C ~ cauchy(0,value_cauchyDF_sigma_r_C);
	sigma_mu_C~cauchy(0,value_cauchyDF_sigma_mu_C);//TB 5/3/2019
	sigma_mu_E~cauchy(0,value_cauchyDF_sigma_mu_E);//TB 5/3/2019 
	//Priors 
	to_vector(eps_C) ~ std_normal();
	to_vector(eps_E) ~ std_normal();
	for(g in 1:G){
    mu_mu_C[g] ~ normal(value_normal_mu_mu_C,value_normal_sigma_mu_C); //TB 5/3/2019
		mu_mu_E[g] ~ normal(value_normal_mu_mu_E,value_normal_sigma_mu_E); //TB 5/3/2019
		for(d in 1:D){
			for(s in 1:S){  					  
				for(i in 1:H){
					eps_E_H[s,i][d,g] ~ gamma(r_E,r_E); 
				}
			}
		}
		for(s in 1:S){
			omega_C_0[g,s] ~ normal(0,sqrt(square(sigma_eps_C)/(1-square(phi_C)))); 
			omega_E_0[g,s] ~ normal(0,sqrt(square(sigma_eps_E)/(1-square(phi_E)))); 
	    eps_mu_C[g,s] ~ std_normal();
			eps_mu_E[g,s] ~ std_normal();
      p_I[g,s] ~ beta(0.5,0.5); 
		}
		R_V[g] ~ beta(0.5,0.5); //Note: leaving constant among days AND sections...may need to tweak; can make beta because is "true" angler cars or angler trailers per angler!
		R_T[g] ~ beta(0.5,0.5); //Note: leaving constant among days AND sections...may need to tweak; can make beta because is "true" angler cars or angler trailers per angler!
		b[g] ~ lognormal(0,value_lognormal_sigma_b); //Note: leaving constant among days AND sections...may need to tweak could go as low as 0.25 for sigma
	}
	//Likelihoods (parallelized via reduce_sum for within-chain threading)
	//Index effort counts - vehicles
	target += reduce_sum(partial_sum_V, V_I, grainsize,
		day_V, section_V, countnum_V,
		lambda_E_S_I, p_TI, R_V, b);
	//Index effort counts - trailers
	target += reduce_sum(partial_sum_T, T_I, grainsize,
		day_T, section_T, countnum_T,
		lambda_E_S_I, p_TI, R_T, b);
	//Index effort counts - anglers
	target += reduce_sum(partial_sum_A, A_I, grainsize,
		day_A, gear_A, section_A, countnum_A,
		lambda_E_S_I, p_TI, p_I);
	//Census (tie-in) effort counts - anglers
	target += reduce_sum(partial_sum_E, E_s, grainsize,
		day_E, gear_E, section_E, countnum_E,
		lambda_E_S_I, p_TI);
	//Angler interviews - CPUE
	target += reduce_sum(partial_sum_IntC, c, grainsize,
		day_IntC, gear_IntC, section_IntC,
		h, lambda_C_S, r_C);
	//Angler interviews - Angler expansions (vehicles + trailers)
	target += reduce_sum(partial_sum_IntA, V_A, grainsize,
		T_A, A_A, gear_IntA,
		R_V, R_T);
}
generated quantities{
  matrix[G*S,G*S] Omega_C; //reconstructed CPUE correlations
  matrix[G*S,G*S] Omega_E; //reconstructed efffort correlations
	array[S] matrix<lower=0>[D,G] lambda_Ctot_S; //total daily catch
	array[S] matrix<lower=0>[D,G] C; //realized total daily catch
	array[S] matrix<lower=0>[D,G] E; //realized total daily effort
	real<lower=0> C_sum; //season-total catch
	real<lower=0> E_sum; //season-total effort
	vector[V_n + T_n + A_n + E_n + IntC + IntA + IntA] log_lik;
	Omega_C = multiply_lower_tri_self_transpose(Lcorr_C);
	Omega_E = multiply_lower_tri_self_transpose(Lcorr_E);
	C_sum = 0;
	E_sum = 0;
	//PPD
	array[V_n] real<lower=0> V_I_rep;
  array[T_n] real<lower=0> T_I_rep;
	array[A_n] real<lower=0> A_I_rep;
	array[E_n] real<lower=0> E_s_rep;
	array[IntC] real<lower=0> c_rep;
	array[IntA] real<lower=0> V_A_rep;
	array[IntA] real<lower=0> T_A_rep;
	
	for(g in 1:G){
		for(d in 1:D){
			for(s in 1:S){
				lambda_Ctot_S[s][d,g] = lambda_E_S[s][d,g] * L[d] * lambda_C_S[s][d,g]; 
				C[s][d,g] = poisson_rng(lambda_Ctot_S[s][d,g]); 
				C_sum = C_sum + C[s][d,g];
				E[s][d,g] = lambda_E_S[s][d,g] * L[d]; 
				E_sum = E_sum + E[s][d,g];
			}							
		}
	}
	//PPD
	//Index effort counts - vehicles
	for(i in 1:V_n){
		V_I_rep[i] = poisson_rng((lambda_E_S_I[section_V[i],countnum_V[i]][day_V[i],1] * p_TI[1,section_V[i]] * R_V[1] +
						 lambda_E_S_I[section_V[i],countnum_V[i]][day_V[i],2] * p_TI[2,section_V[i]] * R_V[2]) * b[1]);  //Note: leaving ratio of cars per angler and bias constant among days since was invariant!
	}
  //Index effort counts - trailers
	for(i in 1:T_n){
		T_I_rep[i] = poisson_rng((lambda_E_S_I[section_T[i],countnum_T[i]][day_T[i],1] * p_TI[1,section_T[i]] * R_T[1] +
						 lambda_E_S_I[section_T[i],countnum_T[i]][day_T[i],2] * p_TI[2,section_T[i]] * R_T[2]) * b[2]); 
	}
  //Index effort counts - anglers
	for(i in 1:A_n){ //KB edit
		A_I_rep[i] = poisson_rng(lambda_E_S_I[section_A[i],countnum_A[i]][day_A[i],gear_A[i]] * p_TI[gear_A[i],section_A[i]] * p_I[gear_A[i],section_A[i]]);
	}
	//Census (tie-in) effort counts - anglers
	for(e in 1:E_n){
		E_s_rep[e] = poisson_rng(lambda_E_S_I[section_E[e],countnum_E[e]][day_E[e],gear_E[e]] * p_TI[gear_E[e],section_E[e]]);				
	}
	//Angler interviews - CPUE
	for(a in 1:IntC){
		c_rep[a] = neg_binomial_2_rng(lambda_C_S[section_IntC[a]][day_IntC[a], gear_IntC[a]] * h[a] , r_C);
	}
	//Angler interviews - Angler expansions
	for(a in 1:IntA){
		//vehicles
		V_A_rep[a] = binomial_rng(A_A[a], R_V[gear_IntA[a]]);  //Note: leaving ratio of cars per angler constant among days since was invariant!
		//trailers
		T_A_rep[a] = binomial_rng(A_A[a], R_T[gear_IntA[a]]);  //Note: leaving ratio of cars per angler constant among days since was invariant!
	}											
	//point-wise log likelihood for LOO-IC
	//Index effort counts - vehicles
	for (i in 1:V_n){
		log_lik[i] = poisson_lpmf(V_I[i]|(lambda_E_S_I[section_V[i],countnum_V[i]][day_V[i],1] * p_TI[1,section_V[i]] * R_V[1] +
						 				  lambda_E_S_I[section_V[i],countnum_V[i]][day_V[i],2] * p_TI[2,section_V[i]] * R_V[2]) * b[1]);
	}
  //Index effort counts - trailers
	for(i in 1:T_n){
		log_lik[V_n +i] = poisson_lpmf(T_I[i]|(lambda_E_S_I[section_T[i],countnum_T[i]][day_T[i],1] * p_TI[1,section_T[i]] * R_T[1] +
						 lambda_E_S_I[section_T[i],countnum_T[i]][day_T[i],2] * p_TI[2,section_T[i]] * R_T[2]) * b[2]); 
	}
  //Index effort counts - anglers
	for(i in 1:A_n){
		log_lik[V_n + T_n + i] = poisson_lpmf(A_I[i]|lambda_E_S_I[section_A[i],countnum_A[i]][day_A[i],gear_A[i]] * p_TI[gear_A[i],section_A[i]] * p_I[gear_A[i],section_A[i]]);
	}
	//Census (tie-in) effort counts - anglers
	for(e in 1:E_n){
		log_lik[V_n + T_n + A_n + e] = poisson_lpmf(E_s[e]|lambda_E_S_I[section_E[e],countnum_E[e]][day_E[e],gear_E[e]] * p_TI[gear_E[e],section_E[e]]);				
	}
	//Angler interviews - catch (number of fish)
	for(a in 1:IntC){
		log_lik[V_n + T_n + A_n + E_n + a] = neg_binomial_2_lpmf(c[a]|lambda_C_S[section_IntC[a]][day_IntC[a],gear_IntC[a]] * h[a] , r_C);
	}
	//Angler interviews - number of vehicles
	for(a in 1:IntA){
		log_lik[V_n + T_n + A_n + E_n + IntC + a] = binomial_lpmf(V_A[a]|A_A[a], R_V[gear_IntA[a]]);
	}
	//Angler interviews - number of trailers
	for(a in 1:IntA){
		log_lik[V_n + T_n + A_n + E_n + IntC + IntA + a] = binomial_lpmf(T_A[a]|A_A[a], R_T[gear_IntA[a]]);
	}												
}
