function [pop_opt, fobj_pop_opt, pop_init, fobj_pop_init] = optim_nsga2(ga_f, pop_size, nb_generation, p_mut, p_cross, Log, param)

[nargout, nargin] = argn();

if ~isdef('param','local') then
  param = [];
end

codage_func        = get_param(param,'codage_func',codage_identity);
init_func          = get_param(param,'init_func',init_func_default);
crossover_func     = get_param(param,'crossover_func',crossover_func_default);
mutation_func      = get_param(param,'mutation_func',mutation_func_default);
selection_strategy = get_param(param,'selection_strategy','elitist');
nb_couples         = get_param(param,'nb_couples',100);

if ~isdef('ga_f','local') then
  error('optim_nsga2: ga_f is mandatory');
end

if ~isdef('pop_size','local') then
  pop_size = 100;
end
if ~isdef('nb_generation','local') then
  nb_generation = 10;
end
if ~isdef('p_mut','local') then
  p_mut = 0.1;
end
if ~isdef('p_cross','local') then
  p_cross = 0.1;
end
if ~isdef('Log','local') then
  Log = %F;
end

// Initialization of the population
if (Log) then
  printf('optim_nsga2: Initialization of the population\n');
end

Pop = init_func(pop_size,param);

if (nargout==4) then
  pop_init = Pop;
end

// Code the individuals
Pop = codage_func(Pop,'code',param);

for i=1:length(Pop)
  FObj_Pop(i,:) = ga_f(Pop(i));
end
MO_FObj_Pop = FObj_Pop;

// Compute the domination rank
Index = 1:size(MO_FObj_Pop,1);
Rank  = zeros(size(MO_FObj_Pop,1),1);
Count = 1;
while size(MO_FObj_Pop,1)>1
  [tmp1,tmp2,Index_List]  = pareto_filter(MO_FObj_Pop);
  Rank(Index(Index_List)) = Count;
  Count = Count + 1;
  MO_FObj_Pop(Index_List,:) = [];
  Index(Index_List) = [];
end

// Compute the crowding distance 
MO_FObj_Pop = FObj_Pop;

Index    = 1:size(MO_FObj_Pop,1);
Crowdist = zeros(size(MO_FObj_Pop,1),1);
for i=1:size(FObj_Pop,2)
  [tmp, Index_List] = sort(MO_FObj_Pop(:,i));
  MO_FObj_Pop       = MO_FObj_Pop(Index_List,:);
  Index             = Index(Index_List);
  Crowdist(Index_List(1)) = %inf;
  Crowdist(Index_List($)) = %inf;
  _Max = max(MO_FObj_Pop(:,i));
  _Min = min(MO_FObj_Pop(:,i));
  for j=2:size(MO_FObj_Pop,1)-1
    Crowdist(Index(j)) = Crowdist(Index(j)) - (MO_FObj_Pop(j+1,i) - MO_FObj_Pop(j-1,i)) / (_Max - _Min);
  end
end

if (nargout==4) then
  fobj_pop_init = FObj_Pop;
end

// The genetic algorithm
for It=1:nb_generation
  if (Log) then
    printf('optim_nsga2: iteration %d / %d\n', It, nb_generation);
  end
  //
  // Selection
  //
  Indiv1 = list();
  Indiv2 = list();
  for j=1:nb_couples
    // Selection of 2 individuals via binary tournament selection to fill Indiv1
    Index1 = ceil((size(FObj_Pop,1) - 1)*rand(1,1)+1);
    Index2 = ceil((size(FObj_Pop,1) - 1)*rand(1,1)+1);
    if (Rank(Index1)<Rank(Index2)) | ((Rank(Index1)==Rank(Index2)) & (Crowdist(Index1)>Crowdist(Index2))) then
      Indiv1(j)        = Pop(Index1);
      FObj_Indiv1(j,:) = MO_FObj_Pop(Index1,:);  
    else
      Indiv1(j)        = Pop(Index2);
      FObj_Indiv1(j,:) = MO_FObj_Pop(Index2,:);  
    end
    // Selection of 2 individuals via binary tournament selection to fill Indiv2
    Index1 = ceil((size(FObj_Pop,1) - 1)*rand(1,1)+1);
    Index2 = ceil((size(FObj_Pop,1) - 1)*rand(1,1)+1);
    if (Rank(Index1)<Rank(Index2)) | ((Rank(Index1)==Rank(Index2)) & (Crowdist(Index1)>Crowdist(Index2))) then
      Indiv2(j)        = Pop(Index1);
      FObj_Indiv2(j,:) = MO_FObj_Pop(Index1,:);  
    else
      Indiv2(j)        = Pop(Index2);
      FObj_Indiv2(j,:) = MO_FObj_Pop(Index2,:);  
    end
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
    if ToCompute_I1(j) then FObj_Indiv1(j,:) = ga_f(Indiv1(j)); end
    if ToCompute_I2(j) then FObj_Indiv2(j,:) = ga_f(Indiv2(j)); end
  end

  // Reinit ToCompute lists
  ToCompute_I1 = ToCompute_I1 & %F;
  ToCompute_I2 = ToCompute_I2 & %F;

  // We merge all the individuals in one list ...  
  All_Pop  = lstcat(Pop, Indiv1, Indiv2);
  All_FObj = [FObj_Pop' FObj_Indiv1' FObj_Indiv2']';  

  MO_All_FObj = All_FObj;

  // Compute the domination rank on all the population
  Index = 1:size(MO_All_FObj,1);
  Rank  = zeros(size(MO_All_FObj,1),1);
  Count = 1;
  while size(MO_All_FObj,1)>1
    [tmp1,tmp2,Index_List]  = pareto_filter(MO_All_FObj);
    Rank(Index(Index_List)) = Count;
    Count = Count + 1;
    MO_All_FObj(Index_List,:) = [];
    Index(Index_List)          = [];
  end

  // Compute the crowding distance
  MO_All_FObj = All_FObj;
  
  Index    = 1:size(MO_All_FObj,1);
  Crowdist = zeros(size(MO_All_FObj,1),1);
  for k=1:size(MO_All_FObj,2)
    [tmp, Index_List] = sort(MO_All_FObj(:,k));
    MO_All_FObj = MO_All_FObj(Index_List,:);
    Index = Index(Index_List);
    Crowdist(Index_List(1)) = %inf;
    Crowdist(Index_List($)) = %inf;
    _Max = max(MO_All_FObj(:,k));
    _Min = min(MO_All_FObj(:,k));
    for j=2:size(MO_All_FObj,1)-1
      Crowdist(Index(j)) = Crowdist(Index(j)) - (MO_All_FObj(j+1,k) - MO_All_FObj(j-1,k)) / (_Max - _Min);
    end
  end
  //
  // Recombination
  //
  // We rank all the individual wrt to the partial order
  for k=1:size(All_FObj,1)-1
    for j=k+1:size(All_FObj,1)
      if (Rank(j)<Rank(k)) | ((Rank(j)==Rank(k)) & (Crowdist(j)>Crowdist(k))) then
        tmp           = Rank(k);
        Rank(k)       = Rank(j);
        Rank(j)       = tmp;
        tmp           = Crowdist(k);
        Crowdist(k)   = Crowdist(j);
        Crowdist(j)   = tmp;
        tmp           = All_Pop(k);
        All_Pop(k)    = All_Pop(j);
        All_Pop(j)    = tmp;
        tmp           = All_FObj(k,:);
        All_FObj(k,:) = All_FObj(j,:);
        All_FObj(j,:) = tmp;
      end
    end
  end
  // Extraction and selection of the phenotype
  FObj_Pop = All_FObj(1:pop_size,:);
  // Extraction and selection of the genotype
  Pop = list(All_Pop(1:pop_size));
  // Extraction of the ranks and Crow distance
  Rank     = Rank(1:pop_size);
  Crowdist = Crowdist(1:pop_size);
end

pop_opt      = codage_func(Pop,'decode',param);
fobj_pop_opt = FObj_Pop;
endfunction