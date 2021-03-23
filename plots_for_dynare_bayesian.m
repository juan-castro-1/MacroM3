function plots_for_dynare_bayesian(plot_options)

% This function creates a graph that combines the impulse responses of
% different models and/or different shocks for teh same model, generates by
% Dynare. If the responses come from different models, it is assumed that
% all the models have the same names for variables and shocks in Dynare.
% The input plot_options is a structure that has to include the following:
%
%   plot_options.model_names: A set of strings containing the mat-file names
%       (without includeing the .mat extension) were the oucomes from dynare 
%       have been stored. The function will create a line in the graph for 
%       each string in model_names.
%   plot_options.marks: A set of strings containg the mark-tipe desired for
%       the line corresponding to each line from model_names.
%   plot_options.u_sel: A set of strings of the same dimension as model_names
%       indicating the name (as declared in dynare) of the exogenous variable whose shock 
%       generates the responses to be ploted.
%   plot_options.v_sel: A set of strings containing the name of the
%       variables (as declared in dynare) whose responses will be displayed.
%       The variables toe be plated will be the same for all the alements in
%       model_name. 
%  plot_options.v_adj: A line vector with the same number of elements as
%       strings in v_sel, indicating the scale adjustment that will be
%       applied for each variable in v_sel.
%  plot_options.v_do_cusum: A line vector with the same number of elements as
%       strings in v_sel, indicating with a 1 if the cummulative impulse
%       response for that variable is required and a 0 if not.
%  plot_options.v_div_ss: A line vector with the same number of elements as
%       strings in v_sel, indicating with a 1 if that variable should be
%       divided by its steady state value and a 0 if not.
%  plot_options.horizon: a numebr indicating the horizon of teh responses to
%       be included in the graphs.
%  plot_options.n_col: a number indicating the number of columns in each the graph.
%  plot_options.n_row: a number indicating the number of rows in each the graph.
%  plot_options.grid: equal to 1 if a grid in each graph is desired, 0 otherwise.
%  plot_options.latex: equal to 1 if a latex format is wanted for titles, 0
%       otherwise. If 1 is selected, the next two have to be included.
%  plot_options.u_name (OPTIONAL): A set of strings of the dimension as u_sel 
%       with the LaTex name for each shock to be included in the title of each graph. 
%       If not included, the name in u_sel is used in the title.
%  plot_options.v_name (OPTIONAL): A set of strings of the dimension as v_sel 
%       with the LaTex name for each variable to be included in the title of each graph. 
%       If not included, the name in v_sel is used in the title.
%
% The function uses the sub-funtion loc defined at the end.

tt=plot_options.horizon;
ir_all=nan(tt,length(plot_options.v_sel));
ir_all_lb=nan(tt,length(plot_options.v_sel));
ir_all_ub=nan(tt,length(plot_options.v_sel));

figure('Name',char(plot_options.u_sel(1)),'units','normalized','outerposition',[0 0 1 1])
set(gcf,'WindowStyle','docked');

for jj=1:length(plot_options.v_sel)
    subplot(plot_options.n_row,plot_options.n_col,jj);
    va = plot_options.v_sel{jj};
    for ii=1:length(plot_options.model_names)
        load([plot_options.model_names{ii} '.mat'])
        sh = plot_options.u_sel{ii};
        if isempty(loc(M_.endo_names,va))==0
            if plot_options.v_div_ss(jj)
                div_ss=abs(oo_.steady_state(loc(M_.endo_names,va)));
            else
                div_ss=1;
            end
%             if plot_options.v_do_cusum(jj)==0
                if plot_options.mean==2
                    ir = getfield(oo_.PosteriorIRF.dsge.Median,[va '_eps_' sh])...
                        *plot_options.v_adj(jj)/div_ss;
                elseif plot_options.mean==1
                    ir = getfield(oo_.PosteriorIRF.dsge.Mean,[va '_eps_' sh])...
                        *plot_options.v_adj(jj)/div_ss;
                elseif plot_options.mean==0
                    ir = [];
                end
                    ir_lb = getfield(oo_.PosteriorIRF.dsge.HPDinf,[va '_eps_' sh])...
                        *plot_options.v_adj(jj)/div_ss;
                    ir_ub = getfield(oo_.PosteriorIRF.dsge.HPDsup,[va '_eps_' sh])...
                        *plot_options.v_adj(jj)/div_ss;
            if plot_options.v_do_cusum(jj)==1
                ir = cumsum(ir);
                ir_lb = cumsum(ir_lb);
                ir_ub = cumsum(ir_ub);
            end
            if plot_options.mean>0
                if max(abs(ir(1:tt)))<1e-10
                    ir_all(:,ii)=0;
                else
                    ir_all(:,ii)=ir(1:tt);
                end
            end
            if max(abs(ir_lb(1:tt)))<1e-10
                ir_all_lb(:,ii)=0;
            else
                ir_all_lb(:,ii)=ir_lb(1:tt);
            end
            if max(abs(ir_ub(1:tt)))<1e-10
                ir_all_ub(:,ii)=0;
            else
                ir_all_ub(:,ii)=ir_ub(1:tt);
            end
        else
            ir_all(:,ii)=0;
            ir_all_ub(:,ii)=0;
            ir_all_lb(:,ii)=0;
        end
    end
    for ii=1:length(plot_options.model_names)
        hold on;
        if plot_options.mean>0
            plot(1:tt,ir_all(:,ii),[plot_options.marks{ii}],'LineWidth',2);
        end
        if plot_options.fill_shade==1
            jbfill([1:tt],ir_all_ub(:,ii)',ir_all_lb(:,ii)','c','c',1,.5);
        else
            plot(1:tt,ir_all_ub(:,ii),[plot_options.marks{ii}],'LineWidth',1);
            plot(1:tt,ir_all_lb(:,ii),[plot_options.marks{ii}],'LineWidth',1);
        end
    end
    if plot_options.grid==1
        grid on;
    end
    if plot_options.latex==1
        va_name=plot_options.v_name{jj};
        sh_name=plot_options.u_name{1};
    else
        va_name=va;
        sh_name=sh;
    end
    title(['$' sh_name ' \Rightarrow ' va_name '$'],'interpreter','latex','FontSize',12);
    set(gca,'FontSize',8,'FontName','Times')
    xlim([1 tt]);
    hold off;
    %     if jj==length(plot_options.v_sel)
    %         expr=['legend('];
    %         for ii=1:length(plot_options.model_names)
    %             if ii==length(plot_options.model_names)
    %                 expr=[expr plot_options.legend_name{ii} ];
    %             else
    %                 expr=[expr plot_options.legend_name{ii} ','];
    %             end
    %         end
    %         expr=[expr ');'];
    %         eval(expr)
    %     end
end


% MATLAB function: loc.m                        July 22, 1992
%        loc(mstring,'sstring') returns the number of the row of
%        mstring that has the same non-blanck characters as
%        sstring. mstring is a matrix of characters. Each of its
%        rows corresponds to a "name". sstring is a character
%        string. It is the "name" we are looking for in mstring.
%        Note that sstring must be placed in between single
%        quotation marks.

function [x] = loc(mstring,sstring,switchmod)

if ischar(mstring)==0
    mstring=char(mstring);
end

[rm,cm]=size(mstring);
cs=max(size(sstring));

% If necessary, add blanck columns to sstring so it will have the
%  same number of columns as mstring.
if cm>cs;
    nblancks=cm-cs;
    for i=1:nblancks
        sstring=[sstring,' '];
    end
end

if(cm~=max(size(sstring)))
    disp(['problem with padding ',sstring])
    disp('The character string might be longer than name list')
    mstring
    %return
    pause
end

x=[];
for r=1:rm;
    if(length(find(mstring(r,:)==sstring))==cm)
%     if(strcmp(mstring(r,:),sstring)==cm)
        x=r;
    end
end

if(x==0)
    if(~exist('switchmod')); disp(['Could not find ',sstring]); end
end


function[fillhandle,msg]=jbfill(xpoints,upper,lower,color,edge,add,transparency)
%USAGE: [fillhandle,msg]=jbfill(xpoints,upper,lower,color,edge,add,transparency)
%This function will fill a region with a color between the two vectors provided
%using the Matlab fill command.
%
%fillhandle is the returned handle to the filled region in the plot.
%xpoints= The horizontal data points (ie frequencies). Note length(Upper)
%         must equal Length(lower)and must equal length(xpoints)!
%upper = the upper curve values (data can be less than lower)
%lower = the lower curve values (data can be more than upper)
%color = the color of the filled area 
%edge  = the color around the edge of the filled area
%add   = a flag to add to the current plot or make a new one.
%transparency is a value ranging from 1 for opaque to 0 for invisible for
%the filled color only.
%
%John A. Bockstege November 2006;
%Example:
%     a=rand(1,20);%Vector of random data
%     b=a+2*rand(1,20);%2nd vector of data points;
%     x=1:20;%horizontal vector
%     [ph,msg]=jbfill(x,a,b,rand(1,3),rand(1,3),0,rand(1,1))
%     grid on
%     legend('Datr')
if nargin<7;transparency=.5;end %default is to have a transparency of .5
if nargin<6;add=1;end     %default is to add to current plot
if nargin<5;edge='k';end  %dfault edge color is black
if nargin<4;color='b';end %default color is blue

if length(upper)==length(lower) && length(lower)==length(xpoints)
    msg='';
    filled=[upper,fliplr(lower)];
    xpoints=[xpoints,fliplr(xpoints)];
    if add
        hold on
    end
    fillhandle=fill(xpoints,filled,color);%plot the data
    set(fillhandle,'EdgeColor',edge,'FaceAlpha',transparency,'EdgeAlpha',transparency);%set edge color
    if add
        hold off
    end
else
    msg='Error: Must use the same number of points in each vector';
end
