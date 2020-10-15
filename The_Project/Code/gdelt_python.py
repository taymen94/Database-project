import pandas as pd
import numpy as np
import sys
if __name__ == '__main__':

    # Reading Datasets
    coronaDf = pd.read_csv('owid-covid-data.csv', parse_dates=True, index_col='date') #covid cases file cleaned
    GM=pd.read_csv('themes_countries/DEU.csv',sep=',',parse_dates=True) # the gdelt data
    US = pd.read_csv('themes_countries/USA.csv', sep=',', parse_dates=True)  # the gdelt data
    IT = pd.read_csv('themes_countries/ITA.csv',  sep=',', parse_dates=True)  # the gdelt data
    print("readed")

    #define the countries we are working on
    countries= ['DEU','USA','ITA']
    countries_gdelt=[GM,US,IT]
    print(GM)
    mask=[3]
    i=1
    while (i <3) :
        covDf = pd.read_csv('owid-covid-data.csv', parse_dates=True, index_col='date')  # covid cases file cleaned
        #extract the data for the wanted countries
        covDf = covDf.loc[(covDf['iso_code'] == countries[i])]
        dfred = countries_gdelt[i]
        print(covDf.columns)
        # Data cleaning and indexing
        covDf.index = pd.to_datetime(covDf.index)
        covDf['date']= covDf.index
        print("aaaaa")

        dfred['date'] = pd.to_datetime(dfred["ts"], format='%Y%m%d%H%M%S')
        dfred['date'] = pd.to_datetime(dfred["date"].dt.strftime('%Y-%m-%d'))
        dfred = dfred.set_index(['date'])
        covDf = covDf.set_index(['date'])
        df_rates = pd.DataFrame(columns=['theme', 'correlation', 'total appearences'])
        #covDf.index.freq = 'D'
        covDf.drop(['iso_code', 'total_cases_per_million', 'new_cases_per_million',
                    'total_deaths_per_million', 'new_deaths_per_million', 'total_tests',
                    'new_tests', 'total_tests_per_thousand', 'new_tests_per_thousand',
                    'tests_units', 'population', 'population_density', 'median_age',
                    'aged_65_older', 'aged_70_older', 'gdp_per_capita', 'extreme_poverty',
                    'cvd_death_rate', 'diabetes_prevalence', 'female_smokers',
                    'male_smokers', 'handwashing_facilities', 'hospital_beds_per_100k'], axis=1, inplace=True)
        #covDf.to_csv('casesGerm.csv', sep='|')
        print("ok_1")
        #dfred = pd.read_csv('themesGerm.csv', sep='|', parse_dates=True)

        print(dfred)
        #covDf=pd.read_csv('casesGerm.csv', sep='|', parse_dates=True)

        j=0
        k=0

        #extracting distinct themes
        distinctThemes = []
        for index, row in dfred.iterrows():
            if row['themes'] not in distinctThemes :
                distinctThemes.append(row['themes'])
        print(len(distinctThemes))

       #iterate over themes and analyse
        for theme in distinctThemes :
          mask=((dfred['themes'] == theme))
          countryDf = dfred.loc[mask]
          covDf = covDf.loc['2020-01-28':'2020-04-30']
          countryDf = countryDf.loc['2020-01-28':'2020-04-30']
          countryDf=countryDf.groupby('date').agg(count=pd.NamedAgg(column='count',aggfunc=sum) )
          print(k,"dimension of covDF : ", covDf.shape[0] , "  dimension of countryDf : ", countryDf.shape[0] )
          if (covDf.shape[0] == countryDf.shape[0]) :
              s3 = pd.concat([covDf, countryDf]).sort_index()
              s3 = pd.merge(covDf, countryDf, how='inner', left_index=True, right_index=True)
              b = False
              h = 0
              for idx, roww in s3.iterrows():
                 h += roww['count']
              tmp = s3['new_cases'].corr(s3['count'])
              print(tmp , h)
              if ((tmp > 0.69) or (tmp < (-0.69)) ) and (h > 1000):
                name = countries[i]+'_' +theme + '.csv'
                s3.to_csv(name, sep='|')
                df_rates.loc[k] = theme, tmp, h
                print(df_rates.loc[k])
                k+=1
          j+=1
        name2='rates_'+countries[i]+'filtered.csv'
        df_rates.to_csv(name2,sep='|')
        print("country nr  done !")
        i+=1