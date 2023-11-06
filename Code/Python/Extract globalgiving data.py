import requests
import pandas as pd
import xml.etree.ElementTree as ET
import json
import pprint
import xmltodict
#xml to json
def load_json(xml_path):
    xml_file = open(xml_path, 'r')
    xml_str = xml_file.read()
    jsondata = xmltodict.parse(xml_str)
    return jsondata
#projects.xml is the data downloaded using the globalgiving api
jsondata=load_json('/Users/liqin/Desktop/projects.xml')
df=pd.DataFrame()
data1=jsondata.get('projects').get('project')
#basedata=data1[30]
for basedata in data1:
    try:
        item=dict()
        item['id']=basedata.get('id')
        item['if_active'] = basedata.get('active')
        item['approvedDate'] = basedata.get('approvedDate')
        item['contactAddress']=basedata.get('contactAddress')
        item['contactCity']=basedata.get('contactCity')
        item['activities'] = basedata.get('activities')
        item['contactCountry']=basedata.get('contactCountry')
        item['contactName']=basedata.get('contactName')
        item['contactpost']=basedata.get('contactPostal')
        item['title'] = basedata.get('title')
        item['summary']=basedata.get('summary')
        item['contactState']=basedata.get('contactState')
        item['country_code']=basedata.get('iso3166CountryCode')
        item['country']=basedata.get('country')
        item['project_goal'] = basedata.get('goal')
        item['project_alreadyfunding'] = basedata.get('funding')
        item['project_remaining'] = basedata.get('remaining')
        item['numberofDonations']=basedata.get('numberOfDonations')
        item['numberOfReports']=basedata.get('numberOfReports')
        imagedata=basedata.get('image').get('imagelink')
        for i in imagedata:
            item['pic%s'%i.get('@size')]=i.get('url')
        item['project_url']=basedata.get('projectLink')
        item['project_Themename']=basedata.get('themeName')
        item['project_status'] = basedata.get('status')
        item['project_desc_challenge'] = basedata.get('need')
        item['project_desc_longTermImpact'] = basedata.get('longTermImpact')
        item['project_desc_solution'] = basedata.get('activities')
        item['modifytime']=basedata.get('modifiedDate')
        item['dateOfMostRecentReport'] = basedata.get('dateOfMostRecentReport')
        try:
            item['lng'] = basedata.get('longitude')
            item['lat'] = basedata.get('latitude')
        except:
            print('no location')
        try:
            theme_list=list()
            theme_list=basedata.get('themes').get('theme')
            if type(theme_list)==dict:
                item['theme_id1']=theme_list.get('id')
                item['theme_name1']=theme_list.get('name')
            else:
                for i,theme_info in enumerate(theme_list,start=0):
                    print(i,theme_info)
                    id_key=f'theme_id{i+1}'
                    name_key=f'theme_name{i+1}'
                    item[id_key]=theme_info['id']
                    item[name_key]=theme_info['name']
        except:
            print('no theme')
        try:
            videodata=list()
            videodata = basedata.get('videos').get('video')
            video_count=1
            for i in videodata:
                item['video_link%d'%video_count]=i.get('url')
                video_count=video_count+1
        except:
            item['video_link']='None'
        try:
            item['organization_id']=basedata.get('organization').get('id')
            item['organization_name'] = basedata.get('organization').get('name')
            item['organization_city'] = basedata.get('organization').get('city')
            item['organization_state'] = basedata.get('organization').get('state')
            item['organization_postal'] = basedata.get('organization').get('postal')
            item['organization_isocode'] = basedata.get('organization').get('iso3166CountryCode')
            item['organization_url'] = basedata.get('organization').get('url')
            item['organization_mission'] = basedata.get('organization').get('mission')
            themeslist=list()
            themeslist=basedata.get('organization').get('themes').get('theme')
            count=1
            for i in themeslist:
                item['organization_themes_id%d'%count]=i.get('id')
                item['organization_themes_name%d'%count] = i.get('name')
                count=count+1
        except:
            print('no organize')
        print(item)
        df=df._append(item, ignore_index=True)
        df.to_csv('/Users/liqin/Desktop/project_detail.csv')
    except:
        print('cannot download')
        df = df._append(item, ignore_index=True)
        df.to_csv('/Users/liqin/Desktop/project_detail.csv')

