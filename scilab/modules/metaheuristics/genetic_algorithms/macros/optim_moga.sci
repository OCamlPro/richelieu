function [pop_opt, fobj_pop_opt, pop_init, fobj_pop_init] = optim_moga(ga_f, pop_size, nb_generation, p_mut, p_cross, Log, param)

[nargout, nargin] = argn();

if ~isdef('param','local') then
  param = [];
end

codage_func    = get_param(param,'codage_func',codage_identity);
init_func      = get_param(param,'init_func',init_func_default);
crossover_func = get_param(param,'crossover_func',crossover_func_default);
mutation_func  = get_param(param,'mutation_func',mutation_func_default);
selection_func = get_param(param,'selection_func',selection_func_elitist);
nb_couples     = get_param(param,'nb_couples',100);
pressure       = get_param(param,'pressure',0.05);

if ~isdef('ga_f','local') then
  error('optim_moga: ga_f is mandatory');
end

if ~isdef('pop_size','local') then
  pop_size = 100;
end
if ~isdef('nb_generation','local') then
  nb_generation = 10;
end
if ~isdef('p_mut','local') then
  p_mut = 0.01;
end
if ~isdef('p_cross','local') then
  p_cross = 0.7;
end
if ~isdef('Log','local') then
  Log = %F;
end

// Initialization of the population
if (Log) then
  printf('optim_moga: Initialization of the population\n');
end

Pop = list();
Pop = init_func(pop_size,param);

if (nargout==4) then
  pop_init = Pop;
end
// Code the individuals
Pop = codage_func(Pop,'code',param);

for i=1:length(Pop)
  MO_FObj_Pop(i,:) = ga_f(Pop(i));
end

// Compute the domination rank
for i=1:size(MO_FObj_Pop,1)
  Index = 0;
  for j=1:size(MO_FObj_Pop,1)
    Index = Index + double(and(MO_FObj_Pop(i,:)<=MO_FObj_Pop(j,:)) & or(MO_FObj_Pop(i,:)<MO_FObj_Pop(j,:)));
  end      
  FObj_Pop(i) = - (Index + 1);
end

FObj_Pop_Max = max(FObj_Pop);
FObj_Pop_Min = min(FObj_Pop);

// Normalization of the efficiency

Efficiency = (1 - pressure) * (FObj_Pop_Max - FObj_Pop) / max([FObj_Pop_Max - FObj_Pop_Min %eps]) + pressure;

if (nargout==4) then
  fobj_pop_init = MO_FObj_Pop;
end

// The genetic algorithm
for i=1:nb_generation
  if (Log) then
    printf('optim_moga: iteration %d / %d', i, nb_generation);
  end
  //
  // Selection
  //
  Indiv1 = list();
  Indiv2 = list();
  Wheel = cumsum(Efficiency);
  for j=1:nb_couples
    // Selection of the first individual in the couple
    Shoot = rand(1,1)*Wheel($);
    Index = 1;
    while((Wheel(Index)<Shoot)&(Index<length(Wheel))) 
      Index = Index + 1;
    end
    Indiv1(j)           = Pop(Index);
    MO_FObj_Indiv1(j,:) = MO_FObj_Pop(Index,:);
    // Selection of the second individual in the couple
    Shoot = rand(1,1)*Wheel($);
    Index = 1;
    while((Wheel(Index)<Shoot)&(Index<length(Wheel))) 
      Index = Index + 1;
    end
    Indiv2(j)           = Pop(Index);
    MO_FObj_Indiv2(j,:) = MO_FObj_Pop(Index,:);
  end
  //
  // Crossover
  //
  for j=1:nb_couples
    if (p_cross>rand(1,1)) then
      [x1, x2] = crossover_func(Indiv1(j), Indiv2(j),param);
      Indiv1(j) = x1;
      Indiv2(j) = x2;
      ToCompute_I1(j) = %T;
      ToCompute_I2(j) = %T;
    else
      ToCompute_I1(j) = %F;
      ToCompute_I2(j) = %F;
    end
  end
  //
  // Mutation
  //
  for j=1:nb_couples
    if (p_mut>rand(1,1)) then
      x1 = mutation_func(Indiv1(j),param);
      Indiv1(j) = x1;
      ToCompute_I1(j) = %T;
    end
    if (p_mut>rand(1,1)) then
      x2 = mutation_func(Indiv2(j),param);
      Indiv2(j) = x2;
      ToCompute_I2(j) = %T;
    end
  end
  //
  // Computation of the objective functions
  //
  for j=1:length(Indiv1)
    if ToCompute_I1(j) then MO_FObj_Indiv1(j,:) = ga_f(Indiv1(j)); end
    if ToCompute_I2(j) then MO_FObj_Indiv2(j,:) = ga_f(Indiv2(j)); end
  end

  // Reinit ToCompute lists
  ToCompute_I1 = ToCompute_I1 & %F;
  ToCompute_I2 = ToCompute_I2 & %F;

  // Compute the domination rank
  for j=1:size(MO_FObj_Indiv1,1)
    // We compute the rank for Indiv1
    Index1 = 0; Index2 = 0; Index3 = 0;
    for k=1:size(MO_FObj_Indiv1,1)
      Index1 = Index1 + double(and(MO_FObj_Indiv1(j,:)<=MO_FObj_Indiv1(k,:)) & or(MO_FObj_Indiv1(j,:)<MO_FObj_Indiv1(k,:)));
      Index2 = Index2 + double(and(MO_FObj_Indiv1(j,:)<=MO_FObj_Indiv2(k,:)) & or(MO_FObj_Indiv1(j,:)<MO_FObj_Indiv2(k,:)));
    end
    for k=1:size(MO_FObj_Pop,1)
      Index3 = Index3 + double(and(MO_FObj_Indiv1(j,:)<=MO_FObj_Pop(k,:)) & or(MO_FObj_Indiv1(j,:)<MO_FObj_Pop(k,:)));
    end
    FObj_Indiv1(j) = - (Index1 + Index2 + Index3 + 1);

    // We compute the rank for Indiv2
    Index1 = 0; Index2 = 0; Index3 = 0;
    for k=1:size(MO_FObj_Indiv1,1)
      Index1 = Index1 + double(and(MO_FObj_Indiv2(j,:)<=MO_FObj_Indiv1(k,:)) & or(MO_FObj_Indiv2(j,:)<MO_FObj_Indiv1(k,:)));
      Index2 = Index2 + double(and(MO_FObj_Indiv2(j,:)<=MO_FObj_Indiv2(k,:)) & or(MO_FObj_Indiv2(j,:)<MO_FObj_Indiv2(k,:)));
    end
    for k=1:size(MO_FObj_Pop,1)
      Index3 = Index3 + double(and(MO_FObj_Indiv2(j,:)<=MO_FObj_Pop(k,:)) & or(MO_FObj_Indiv2(j,:)<MO_FObj_Pop(k,:)));
    end
    FObj_Indiv2(j) = - (Index1 + Index2 + Index3 + 1);
  end
  
  // We compute the rank for Pop
  for j=1:size(MO_FObj_Pop,1)
    Index1 = 0; Index2 = 0; Index3 = 0;
    for k=1:size(MO_FObj_Indiv1,1)
      Index1 = Index1 + double(and(MO_FObj_Pop(j,:)<=MO_FObj_Indiv1(k,:)) & or(MO_FObj_Pop(j,:)<MO_FObj_Indiv1(k,:)));
      Index2 = Index2 + double(and(MO_FObj_Pop(j,:)<=MO_FObj_Indiv2(k,:)) & or(MO_FObj_Pop(j,:)<MO_FObj_Indiv2(k,:)));
    end
    for k=1:size(MO_FObj_Pop,1)
      Index3 = Index3 + double(and(MO_FObj_Pop(j,:)<=MO_FObj_Pop(k,:)) & or(MO_FObj_Pop(j,:)<MO_FObj_Pop(k,:)));
    end
    FObj_Pop(j) = - (Index1 + Index2 + Index3 + 1);
  end

  //
  // Recombination
  //
  
  [Pop,FObj_Pop,Efficiency,MO_FObj_Pop] = selection_func_elitist(Pop,Indiv1,Indiv2,FObj_Pop,FObj_Indiv1,FObj_Indiv2, ...
                                                                 MO_FObj_Pop,MO_FObj_Indiv1,MO_FObj_Indiv2,param);
  if (Log) then
    printf(' - min / max value found = %f / %f\n', min(FObj_Pop), max(FObj_Pop));
  end
end

pop_opt      = codage_func(Pop,'decode',param);
fobj_pop_opt = MO_FObj_Pop;
endfunction