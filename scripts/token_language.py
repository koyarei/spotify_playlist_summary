# This script queries uClassify API to identify token language
# in Spotify playlist data
import requests
import json
import pandas as pd
import re

API_KEY = "WySmXy7wUGuH"

def get_language(response):
	js = json.loads(response)
	classification = js[0]
	langs = classification['classification']
	df = pd.DataFrame(langs)
	best_lang = df[df.p == df.p.max()].className
	best_lang = best_lang if len(best_lang) == 1 else "unknown"
	final_lang = clean_lang(str(best_lang))
	print final_lang
	return final_lang

def query_api(text):
	response = requests.post('https://api.uclassify.com/v1/uclassify/text language/classify', \
           data = "{\"texts\": [\"%s\"]}" % text, \
           headers = {'Authorization': 'Token ' + API_KEY}) 
	return get_language(response.content)

def clean_lang(lang):
	line = re.sub(r"\d+\s+", "", lang)
	clean_lang = line.replace("\nName: className, dtype: object","")
	return clean_lang


################################ Main #############################
data = pd.read_csv('/Users/Koya/projects/spotify_playlist_summary/data/playlist.update.5.csv')
tokens = data.tokens

results = []
for idx, text in enumerate(tokens):
	results.append(query_api(text))
	print 403365-idx-1

token_langs = pd.DataFrame({'token': tokens,
	                        'language': results})






