import pandas as pd
import datetime

# GDP data to wide_df
wide_df = pd.read_csv('/Users/liqin/Desktop/UN数据集/GDPcsv.csv')
long_df = pd.melt(wide_df, id_vars=['Country_Code'], var_name='Year', value_name='GDP')
long_df.to_csv('/Users/liqin/Desktop/UN数据集/gdp2003-2022.csv')
print(long_df)
# count Countries
unique_country_codes = long_df['Country_Code'].nunique()
# 计算数据的年份跨度
min_year = long_df['Year'].min()
max_year = long_df['Year'].max()
#264个国家 2003-2022数据

df = pd.read_csv('/Users/liqin/Desktop/UN数据集/project_detail.csv')
# match country_code
lookup_df = pd.read_csv('/Users/liqin/Desktop/UN数据集/codefilter.csv')
merged_df = pd.merge(df, lookup_df, left_on='country_code', right_on='code2', how='left')
merged_df.rename(columns={'code3': 'country_code3'}, inplace=True)
merged_df2 = pd.merge(merged_df, lookup_df, left_on='organization_isocode', right_on='code2', how='left')
merged_df2.rename(columns={'code3': 'organization_code3'}, inplace=True)
merged_df2 = merged_df2.drop(['code2_x', 'countryname_x', 'code2_y', 'countryname_y'], axis=1)

# date to_datetime
# start time
merged_df2['approvedDate'] = merged_df2['approvedDate'].astype(str)
merged_df2['date1'] = merged_df2['approvedDate'].str.slice(stop=10)
merged_df2['date1'] = pd.to_datetime(merged_df2['date1'])
merged_df2['year'] = merged_df2['date1'].dt.year

# end time
merged_df2['dateOfMostRecentReport'] = merged_df2['dateOfMostRecentReport'].astype(str)
merged_df2['date_end'] = merged_df2['dateOfMostRecentReport'].str.slice(stop=10)
merged_df2['date_end'] = pd.to_datetime(merged_df2['date_end'])
# compute time interval
merged_df2['interval_day'] = (merged_df2['date_end'] - merged_df2['date1']).dt.days
merged_df2['interval_month'] = merged_df2['interval_day'] / 30

# match gdp
# gdptable:gdp_df
gdp_df = pd.read_csv('/Users/liqin/Desktop/UN数据集/gdp2003-2022.csv')
# merge by country and year
merged_df2 = pd.merge(merged_df2, gdp_df, left_on=['country_code3', 'year'], right_on=['Country_Code', 'Year'],
                      how='left')
# 删除多余的列（Country_Code, Year）
merged_df2 = merged_df2.drop(['Country_Code', 'Year'], axis=1)
merged_df2.rename(columns={'GDP': 'country_gdp'}, inplace=True)  # country_gdp is the target area's gdp

merged_df2 = pd.merge(merged_df2, gdp_df, left_on=['organization_code3', 'year'], right_on=['Country_Code', 'Year'],
                      how='left')
merged_df2 = merged_df2.drop(['Country_Code', 'Year'], axis=1)
merged_df2.rename(columns={'GDP': 'organization_country_gdp'},
                  inplace=True)  # organization_gdp is the organization area's gdp

# merge the table of developed countries
lookup_df2 = pd.read_csv(
    '/Users/liqin/Desktop/UN数据集/income_class.csv')  # table of developed countries,Manually merging upper-middle income countries and low-income countries into middle-income countries
merged_df3 = pd.merge(merged_df2, lookup_df2, left_on='country_code3', right_on='Country_Code', how='left')
merged_df3.rename(columns={'Income_Group': 'country_group'},
                  inplace=True)  # country_group is the target area's degree of development
merged_df3.drop(['Country_Code', 'Country Name'], axis=1, inplace=True)
merged_df4 = pd.merge(merged_df3, lookup_df2, left_on='organization_code3', right_on='Country_Code', how='left')
merged_df4.rename(columns={'Income_Group': 'organization_country_group'},
                  inplace=True)  # organization_country_group is the organization area's degree of development
merged_df4.drop(['Country_Code', 'Country Name'], axis=1, inplace=True)

# Combine latitude and longitude data
lookup_df3 = pd.read_csv('/Users/liqin/Desktop/UN数据集/countries.csv')
merged_df5 = pd.merge(merged_df4, lookup_df3, left_on='country_code', right_on='ISO', how='left')
merged_df5.rename(columns={'longitude': 'country_longitude'},
                  inplace=True)  # country_longitude is the target area's longitude
merged_df5.rename(columns={'latitude': 'country_latitude'},
                  inplace=True)  # country_latitude is the target area's latitude
merged_df5.drop(['COUNTRY', 'ISO', 'COUNTRYAFF', 'AFF_ISO'], axis=1, inplace=True)
merged_df6 = pd.merge(merged_df5, lookup_df3, left_on='organization_isocode', right_on='ISO', how='left')
merged_df6.rename(columns={'longitude': 'organization_country_longitude'},
                  inplace=True)  # organization_country_longitude is the organization area's longitude
merged_df6.rename(columns={'latitude': 'organization_country_latitude'},
                  inplace=True)  # organization_country_latitude is the organization area's latitude
merged_df6.drop(['COUNTRY', 'ISO', 'COUNTRYAFF', 'AFF_ISO'], axis=1, inplace=True)
merged_df6.drop(['Unnamed: 0'], axis=1, inplace=True)

# The null value (NA) is recognized as the exception country code NAM, so this part is eliminated,organization.The column of ['organization_code3'] == 'NAM' has no organization-related data itself
merged_df6.loc[merged_df6['organization_code3'] == 'NAM', ['organization_country_gdp', 'organization_country_group',
                                                           'organization_country_longitude',
                                                           'organization_country_latitude']] = ''
merged_df6.loc[merged_df6['organization_code3'] == 'NAM', 'organization_code3'] = ''

# Determine whether the project sponsor and project recipient countries are consistent
merged_df6['remote'] = (merged_df6['organization_isocode'] != merged_df6['country_code']) & \
                       (merged_df6['organization_isocode'].notna())
# If organization_isocode is empty, merged_df6['remote'] is empty
merged_df6.loc[merged_df6['organization_isocode'].isna(), 'remote'] = ''
merged_df6.to_csv('/Users/liqin/Desktop/UN数据集/new/未删除列处理后总表1105.csv', index=False)

# Filter out the projects that have finished
filtered_data1 = merged_df6[merged_df6['if_active'] == False]
filtered_data1.to_csv('/Users/liqin/Desktop/UN数据集/new/table1非活跃项目总表1105.csv', index=False)

# Filter out the important columns
subset = ['id', 'if_active', 'approvedDate', 'date1', 'date_end', 'year', 'interval_day', 'interval_month', 'title',
          'country_code', 'country', 'country_gdp', 'organization_country_gdp', 'project_goal',
          'project_alreadyfunding', 'project_remaining', 'numberofDonations',
          'numberOfReports', 'project_status', 'dateOfMostRecentReport', 'lng', 'lat', 'organization_id',
          'organization_name', 'organization_isocode', 'country_code3', 'organization_code3',
          'project_Themename', 'project_url', 'country_group', 'organization_country_group', 'country_longitude',
          'country_latitude', 'organization_country_longitude',
          'organization_country_latitude', 'remote']
data3 = filtered_data1[subset]
# Clear the row where 'organization_isocode','country_code' are NA
data3.dropna(subset=['organization_isocode', 'country_code'], inplace=True)
data3.to_csv('/Users/liqin/Desktop/UN数据集/new/table2发起和异地均不为空的未在进行中的项目.csv')

# Output tables based on year, project_Themename, and remote
filtered_data = filtered_data1
grouped_df = filtered_data.groupby('year')
for year, data in grouped_df:
    year_data = grouped_df.get_group(year)
    year_data.to_csv(f'/Users/liqin/Desktop/UN数据集/new/按照table1分析/year/{year}.csv', index=False)
grouped_df = filtered_data.groupby('project_Themename')
for theme, data in grouped_df:
    year_data = grouped_df.get_group(theme)
    year_data.to_csv(f'/Users/liqin/Desktop/UN数据集/new/按照table1分析/theme/{theme}.csv', index=False)
grouped_df = filtered_data.groupby('remote')
for remote, data in grouped_df:
    year_data = grouped_df.get_group(remote)
    year_data.to_csv(f'/Users/liqin/Desktop/UN数据集/new/按照table1分析/remote/{remote}.csv', index=False)

# Calculate the number of projects, the number of donors, the per capita donations, and the total amount of donations each year by running the following two files respectively
data = pd.read_csv(
    '/Users/liqin/Desktop/UN数据集/new/未删除列处理后总表1105.csv')  # To run this line, ignore the next line
data = pd.read_csv('/Users/liqin/Desktop/UN数据集/new/table2发起和异地均不为空的未在进行中的项目.csv')
# (1) Categorize the total amount donated each year
total_funding = data.groupby('year')['project_alreadyfunding'].sum().reset_index()
# (2) Calculate the total number of projects per year
project_count = data.groupby('year')['id'].count().reset_index()
# (3) Summarize the total number of project donors each year
total_donors = data.groupby('year')['numberofDonations'].sum().reset_index()
# (4) Calculate per capita donations per year
average_donation = total_funding['project_alreadyfunding'] / total_donors['numberofDonations']
result = pd.DataFrame({
    'year': total_funding['year'],
    'Total Funding': total_funding['project_alreadyfunding'],
    'Project Count': project_count['id'],
    'Total Donors': total_donors['numberofDonations'],
    'Average Donation': average_donation
})
result.to_csv('/Users/liqin/Desktop/UN数据集/new/40000+统计.csv')
result.to_csv('/Users/liqin/Desktop/UN数据集/new/table2统计.csv')

# Supplementary data ITU
# merge ITU data.Run the following two files separately
data = pd.read_csv(
    '/Users/liqin/Desktop/UN数据集/new/未删除列处理后总表1105.csv')  # To run this line ignore the next line
data = pd.read_csv('/Users/liqin/Desktop/UN数据集/new/table2发起和异地均不为空的未在进行中的项目.csv')
itc_df = pd.read_csv('/Users/liqin/Desktop/UN数据集/ICT.csv')
merged_df_itu = pd.merge(data, itc_df, left_on=['country_code3', 'year'], right_on=['ISO', 'Year'], how='left')
merged_df_itu.drop(['Year', 'ISO', 'Indicator name', 'Region'], axis=1, inplace=True)
merged_df_itu.rename(columns={'Value': 'country_International_bandwidth_per_Internet_user'},
                     inplace=True)  # country_International_bandwidth_per_Internet_user is the target area's International bandwidth per Internet user (kbit/s)
merged_df_itu2 = pd.merge(merged_df_itu, itc_df, left_on=['organization_code3', 'year'], right_on=['ISO', 'Year'],
                          how='left')
merged_df_itu2.drop(['Year', 'ISO', 'Indicator name', 'Region'], axis=1, inplace=True)
merged_df_itu2.rename(columns={'Value': 'organization_country_International_bandwidth_per_Internet_user'},
                      inplace=True)  # organization_country_International_bandwidth_per_Internet_user is the organization area's International bandwidth per Internet user (kbit/s)
merged_df_itu2.to_csv('/Users/liqin/Desktop/UN数据集/new/未删除列处理后总表1105_itu.csv')
merged_df_itu2.to_csv('/Users/liqin/Desktop/UN数据集/new/table2发起和异地均不为空的未在进行中的项目_itu.csv')

#translate
# In columns country_group and organization_country_group, with fields for middle-income, high-income, and low-income countries ( in Chinese), translate those fields into English and save the table again
data=pd.read_csv('/Users/liqin/Desktop/UN数据集/new/未删除列处理后总表1105_itu.csv')
data=pd.read_csv('/Users/liqin/Desktop/UN数据集/new/table2发起和异地均不为空的未在进行中的项目_itu.csv')
translation_dict = {
    '中等收入国家': 'Middle-Income Country',
    '高收入国家': 'High-Income Country',
    '低收入国家': 'Low-Income Country'
}
data['country_group_translate'] = data['country_group'].replace(translation_dict)
data['organization_country_group_translate'] = data['organization_country_group'].replace(translation_dict)
data.to_csv('/Users/liqin/Desktop/UN数据集/new/table2发起和异地均不为空的未在进行中的项目_itu2.csv', index=False)