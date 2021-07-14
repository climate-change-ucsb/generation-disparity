clear
data=readtable('ctytemp.csv','ReadRowNames',true,'PreserveVariableNames',true,'TreatAsEmpty','NA');
data2=readmatrix('ctytemp.csv');
temp=data2(:,2:337);
name=data.Properties.VariableNames;
rcp26=contains(name,'rcp26b');
rcp45=contains(name,'rcp45b');
rcp60=contains(name,'rcp60b');
rcp85=contains(name,'rcp85b');

country=shaperead('gadm28_levels.shp/gadm28_adm0.shp');
listcountry={country.ISO};
countryselect=readtable('/Users/haozheyang/Documents/GitHub/cscc-paper-2018-master/BHM_rcp60 _ SSP1 _ .csv');
select=countryselect.Var2(2:end);

listcountry=listcountry';

position=zeros(length(listcountry),1);
for i=1:length(select)
    st=select{i};
    position=strcmp(st,listcountry)+position;
end
position=logical(position);
countrynew=listcountry(position);

[A,B]=sort(countrynew);

diffrcp26=mean(temp(position,rcp26),2,'omitnan');
diffrcp26_input=diffrcp26(B);
mean(diffrcp26,'omitnan')

diffrcp45=mean(temp(position,rcp45),2,'omitnan');
diffrcp45_input=diffrcp45(B);
mean(diffrcp45,'omitnan')

diffrcp60=mean(temp(position,rcp60),2,'omitnan');
diffrcp60_input=diffrcp60(B);
mean(diffrcp60,'omitnan')

diffrcp85=mean(temp(position,rcp85),2,'omitnan');
diffrcp85_input=diffrcp85(B);
mean(diffrcp85,'omitnan')

basetemp=readtable('basetemp.csv');
tempname=basetemp.ISO3;
[tempname,tempname_order]=sort(tempname);

tempname_select=contains(tempname,A);
base_temp=basetemp.basetemp(tempname_order);
base_temp=base_temp(tempname_select);

temp_rcp26_project=zeros(169,81);
temp_rcp45_project=zeros(169,81);
temp_rcp60_project=zeros(169,81);
temp_rcp85_project=zeros(169,81);

for i=10:90
temp_rcp26_project(:,i-9)=diffrcp26_input/90*i+base_temp;
temp_rcp45_project(:,i-9)=diffrcp45_input/90*i+base_temp;
temp_rcp60_project(:,i-9)=diffrcp60_input/90*i+base_temp;
temp_rcp85_project(:,i-9)=diffrcp85_input/90*i+base_temp;
end

temp_=temp_rcp26_project';
rcp26_country=temp_(:);

temp_=temp_rcp45_project';
rcp45_country=temp_(:);

temp_=temp_rcp60_project';
rcp60_country=temp_(:);

temp_=temp_rcp85_project';
rcp85_country=temp_(:);
countryleveltmp=[rcp26_country rcp45_country rcp60_country rcp85_country];

%csvwrite('temp.csv',countryleveltmp);
base_temp_input=zeros(169*81,1);

for i=1:169
base_temp_input((i-1)*81+1:81*i,1)=base_temp(i);
end
