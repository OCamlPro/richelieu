// example of use of the genetic algorithm on a 2D Ising model

// Loading test problems
getd('../../test_problems/Ising');
// Load the crossover and mutation operators for the Ising 2D problem
getd('.');

J       = 1.1;
H       = 0.7;
Magnet  = '%T';
Connect = '%T';

// For the minimization case, everything must be at -1 or 1 in the optimal solution
//deff('y=f(x)','y = ising2d(x,'+string(J)+','+string(H)+','+Magnet+','+Connect+')');
// For the maximization case, we must have a checker board solution (+1 -1 +1 -1 ....)
deff('y=f(x)','y = - ising2d(x,'+string(J)+','+string(H)+','+Magnet+','+Connect+')');

PopSize     = 100;
Proba_cross = 0.7;
Proba_mut   = 0.1;
NbGen       = 10;
NbCouples   = 110;
Log         = %T;
nb_disp     = 10; // Nb point to display from the optimal population
pressure    = 0.05;
DisplayIndiv = %T;

ga_params = init_param();
// Parameters to adapt to the shape of the optimization problem
ga_params = add_param(ga_params,'dimension',10);
ga_params = add_param(ga_params,'proba',0.05);

// Parameters to fine tune the Genetic algorithm. All these parameters are optional for continuous optimization
// If you need to adapt the GA to a special problem, you 
ga_params = add_param(ga_params,'init_func',init_func_ising2d);
ga_params = add_param(ga_params,'crossover_func',crossover_func_ising2d);
ga_params = add_param(ga_params,'mutation_func',mutation_func_ising2d);
ga_params = add_param(ga_params,'codage_func',codage_identity);
ga_params = add_param(ga_params,'selection_func',selection_func_elitist);
//ga_params = add_param(ga_params,'selection_func',selection_func_random);
ga_params = add_param(ga_params,'nb_couples',NbCouples);
ga_params = add_param(ga_params,'pressure',pressure);

///////////////////////
// Genetic Algorithm //
///////////////////////

[pop_opt, fobj_pop_opt, pop_init, fobj_pop_init] = optim_ga(f, PopSize, NbGen, Proba_mut, Proba_cross, Log, ga_params);

printf('Genetic Algorithm: %d points from pop_opt\n', nb_disp);
for i=1:nb_disp 
  printf('Individual %d: f = %f\n', i, fobj_pop_opt(i));
  if DisplayIndiv then disp(pop_opt(i)); end
end
