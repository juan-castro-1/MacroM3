clear;
clc;
close all;


% Opciones
plot_options.model_names={'Uribe_arg_results'}; % nombre de los .mat creados al final del correr Dynare correspondientes a los distintos modelos
shocks        ={'xm'          ,'zm'   ,'xy'          ,'zy'}; % nombre de variables shockeadas, con el mismo nombre que el declarado en Dynare (este programa asume que si la vraiable es, por ejemplo, 'a', el shocks es llamado 'u_a' en el mod-file)
shocks_latex  ={'\Delta X^m_t','z^m_t','\Delta X^n_t','z^n_t'}; % nombre que aparecera en el titulo de cada IRF para cada variable ex�gena. Se permite escribir en formato LaTex

v_sel         = { 'y' ,'ii' ,'ppi'  ,'rexp'}; % nombre de las variables cuyo irf desean mostrarse, con el mismo nombre que el declarado en Dynare
v_name        = {'y_t','i_t','\pi_t','i_t-\pi_{t+1}'};% nombre que aparecera en el titulo de cada IRF para cada variable ex�gena. Se permite escribir en formato LaTex.
v_do_cusum    = [    0,   0 ,      0,     0 ]; % Vector (de tamanio igual al numero de variables +1) con un 1 en la posicion de la variable en la cual se desea desplegar la IRF acumulada (util cuando la variable es una tasa de crecimiento). En este caso, son todos ceros porque no se desea esto (el +1, aqui y abajo, es porque tambien se grafica la IRF de la variable exogena). 

plot_options.n_col      = 3;
plot_options.n_row      = 2;

plot_options.mean       = 2; % 1 if show posterior mean, 2 if median, 0 if no  point estimate is to be shown

plot_options.fill_shade  = 1; % 1 if credible set is fill with grey collor, 0 if uses the same markers as the IRF mean

plot_options.v_do_cusum = [v_do_cusum 0];
plot_options.v_adj      = ones(1,size(v_sel,2)+1);
plot_options.v_div_ss   = zeros(1,size(v_sel,2)+1);
plot_options.horizon    = 12;
plot_options.grid       = 1;
plot_options.latex      = 1;
plot_options.marks={'-b','--r','-.k',':m'};

plot_options.saving=0; % opcion que permite guardar cada uno de los grafico en el current folder. Si es 0 no guarda nada, si se 1 guarda en formato eps, si es 2 en formato pdf, si es 3 en formato jpeg. El nombre del archivo creado esta identificado por el nombre del shock.

% De aqui en mas no tocar nada.
for j=1:size(shocks,2)
    plot_options.u_sel=shocks(j);
    if size(plot_options.model_names,2)>1
        for i=2:size(plot_options.model_names,2)
            plot_options.u_sel=[plot_options.u_sel shocks(j)];
        end
    end
    plot_options.u_name=shocks_latex(j);
	plot_options.v_sel=[v_sel shocks(j)];
    plot_options.v_name=[ v_name shocks_latex(j)];
    plots_for_dynare_bayesian(plot_options)
    if plot_options.saving==1
        saveas(gcf,['irfbayes_' shocks{j} '.eps'],'psc2');
    elseif plot_options.saving==2
        saveas(gcf,['irfbayes_' shocks{j} '.pdf'],'pdf');
    elseif plot_options.saving==3
        saveas(gcf,['irfbayes_' shocks{j} '.jpg'],'jpg');
    end    
end
