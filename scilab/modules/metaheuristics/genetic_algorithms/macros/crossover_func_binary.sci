function [Crossed_Indiv1, Crossed_Indiv2] = crossover_func_binary(Indiv1,Indiv2,param)
if ~isdef('param','local') then
  param = [];
end

// We deal with some parameters to take into account the boundary of the domain and the neighborhood size
BinLen       = get_param(param,'binary_length',8);
MultiCross   = get_param(param,'multi_cross',%F);
MultiCrossNb = get_param(param,'multi_cross_nb',2);

if ~MultiCross then
  mix = ceil((length(Indiv1)-1)*rand(1,1))+1;

  part1_1 = part(Indiv1,1:mix);
  part1_2 = part(Indiv1,mix+1:length(Indiv1));
  part2_1 = part(Indiv2,1:mix);
  part2_2 = part(Indiv2,mix+1:length(Indiv2));

  Crossed_Indiv1 = strcat([part1_1 part2_2]);
  Crossed_Indiv2 = strcat([part1_2 part2_1]);
else
  mix = ceil((length(Indiv1)-1)*rand(MultiCrossNb,1))+1;
  mix = -unique(sort(-mix));
  Crossed_Indiv1 = Indiv1;
  Crossed_Indiv2 = Indiv2;
  for i=1:length(mix)
    part1_1 = part(Crossed_Indiv1,1:mix(i));
    part1_2 = part(Crossed_Indiv1,mix(i)+1:length(Crossed_Indiv1));
    part2_1 = part(Crossed_Indiv2,1:mix(i));
    part2_2 = part(Crossed_Indiv2,mix(i)+1:length(Crossed_Indiv2));

    Crossed_Indiv1 = strcat([part1_1 part2_2]);
    Crossed_Indiv2 = strcat([part1_2 part2_1]);
  end
end
endfunction