// example of use of the genetic algorithm

// Load test problems
getf('../../test_problems/cont_funcs.sci');

//func = 'rosenbrock';
func = 'mccormic';
//func = 'sixhumpcamelb';
//func = 'branin2';
//func = 'schubert';
//func = 'hansen';
//func = 'paviani';
//func = 'booth';
//func = 'matyas';
//func = 'rastrigin';
//func = 'griewank2';
//func = 'exp2';
//func = 'treccani';
//func = 'branin';
//func = 'colville';
//func = 'chichinadze';
//func = 'hartmann34';
//func = 'hartmann64';
//func = 'price';
//func = 'goldsteinprice';
//func = 'dixonprice';
//func = 'hump';
//func = 'dejongf2';
//func = 'dejongf5';
//func = 'dejongf7';
//func = 'schafferf6'
//func = 'schafferf7';
//func = 'stuckman';
//func = 'easom';
//func = 'bohachevsky1';
//func = 'bohachevsky2';
//func = 'bohachevsky3';
//func = 'beale';
//func = 'levy13';
//func = 'levy8';
//func = 'levy5';
//func = 'levy2';
//func = 'holtzmann';
//func = 'gen_rosen';
//func = 'shekel';
//func = 'griewank';
//func = 'sphere';
//func = 'weierstrass';
//func = 'ackley';
//func = 'ellipsoid';
//func = 'rotell';
//func = 'abspow';
//func = 'michalewicz';
//func = 'powell';
//func = 'power';
//func = 'gen_rastrigin';
//func = 'schwefel';
//func = 'trid';
//func = 'zhakarov';
//func = 'freudroth';
//func = 'himmelblau';
//func = 'jensamp';
//func = 'zhufu';
//func = 'cola';
//func = 'leon';
//func = 'giunta';
//func = 'bukin2';
//func = 'bukin4';
//func = 'bukin6';
//func = 'stybtang';
//func = 'zettl';
//func = 'threehumpcamelb';

deff('y=f(x)','y = '+func+'(x)');

PopSize     = 100;
Proba_cross = 0.7;
Proba_mut   = 0.1;
NbGen       = 10;
NbCouples   = 110;
Log         = %T;
nb_disp     = 10; // Nb point to display from the optimal population
pressure    = 0.05;

ga_params = init_param();
// Parameters to adapt to the shape of the optimization problem
ga_params = add_param(ga_params,'minbound',eval('min_bd_'+func+'()'));
ga_params = add_param(ga_params,'maxbound',eval('max_bd_'+func+'()'));
ga_params = add_param(ga_params,'dimension',2);
ga_params = add_param(ga_params,'beta',0);
ga_params = add_param(ga_params,'delta',0.1);
// Parameters to fine tune the Genetic algorithm. All these parameters are optional for continuous optimization
// If you need to adapt the GA to a special problem, you 
ga_params = add_param(ga_params,'init_func',init_func_default);
ga_params = add_param(ga_params,'crossover_func',crossover_func_default);
ga_params = add_param(ga_params,'mutation_func',mutation_func_default);
ga_params = add_param(ga_params,'codage_func',codage_identity);
ga_params = add_param(ga_params,'selection_func',selection_func_elitist);
//ga_params = add_param(ga_params,'selection_func',selection_func_random);
ga_params = add_param(ga_params,'nb_couples',NbCouples);
ga_params = add_param(ga_params,'pressure',pressure);

Min = get_param(ga_params,'minbound');
Max = get_param(ga_params,'maxbound');
x0  = (Max - Min) .* rand(size(Min,1),size(Min,2)) + Min;

//////////////////////////////////////////

x = Min(1):(Max(1)-Min(1))/20:Max(1); y = Min(2):(Max(2)-Min(2))/20:Max(2);
[X,Y]=meshgrid(x,y);
for i=1:size(X,1)
  for j=1:size(X,2)
    Z(i,j) = eval(func+'([X(i,j) Y(i,j)])');
  end
end

scf();
drawlater;
xset('fpf',' ');
contour(x,y,Z', 10);
_axes = get("current_axes");
_axes.data_bounds = [Min(1) Max(1) Min(2) Max(2)];
xtitle('Genetic Algorithm','x1','x2');

drawnow;

///////////////////////
// Genetic Algorithm //
///////////////////////

[pop_opt, fobj_pop_opt, pop_init, fobj_pop_init] = optim_ga(f, PopSize, NbGen, Proba_mut, Proba_cross, Log, ga_params);

if (size(pop_opt(1)',2)==2) then
  drawlater;
  printf('plotting init population ...\n');
  for i=1:length(pop_init)
    plot(pop_init(i)(1),pop_init(i)(2),'r.');
  end
  printf('plotting result population ...\n');
  for i=1:length(pop_opt)
    plot(pop_opt(i)(1),pop_opt(i)(2),'g.');
  end
  drawnow;
end

printf('Genetic Algorithm: %d points from pop_opt\n', nb_disp);
for i=1:nb_disp 
  printf('Individual %d: x(1) = %f x(2) = %f -> f = %f\n', i, pop_opt(i)(1), pop_opt(i)(2), fobj_pop_opt(i));
end

/////////////////////////////////////////
// Genetic Algorithm for binary codage //
/////////////////////////////////////////

deff('y=f(x)','BinLen = get_param(ga_params,''binary_length''); ...
               tmp    = convert_to_float(x, BinLen, Max, Min); ...
               y      = '+func+'(tmp);','n');

ga_params = add_param(ga_params,'binary_length',8);
ga_params = set_param(ga_params,'crossover_func',crossover_func_binary);
ga_params = set_param(ga_params,'mutation_func',mutation_func_binary);
ga_params = set_param(ga_params,'codage_func',codage_binary);
ga_params = add_param(ga_params,'multi_cross',%T);
ga_params = add_param(ga_params,'multi_cross_nb',3);

[pop_opt, fobj_pop_opt, pop_init, fobj_pop_init] = optim_ga(f, PopSize, NbGen, Proba_mut, Proba_cross, Log, ga_params);

if (size(pop_opt(1)',2)==2) then
  scf();
  drawlater;
  xset('fpf',' ');
  contour(x,y,Z', 10);
  _axes = get("current_axes");
  _axes.data_bounds = [Min(1) Max(1) Min(2) Max(2)];
  xtitle('Genetic Algorithm - Binary','x1','x2');
  
  printf('plotting init population ...\n');
  for i=1:length(pop_init)
    plot(pop_init(i)(1),pop_init(i)(2),'r.');
  end
  printf('plotting result population ...\n');
  for i=1:length(pop_opt)
    plot(pop_opt(i)(1),pop_opt(i)(2),'g.');
  end
  drawnow;
end

printf('Genetic Algorithm - binary: %d points from pop_opt\n', nb_disp); 
for i=1:nb_disp 
  printf('Individual %d: x(1) = %f x(2) = %f -> f = %f\n', i, pop_opt(i)(1), pop_opt(i)(2), fobj_pop_opt(i));
end