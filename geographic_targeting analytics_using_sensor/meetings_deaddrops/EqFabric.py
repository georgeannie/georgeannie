import requests
import yaml
import base64
import pandas as pd
import json
import time

import urllib3
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

RE = requests.exceptions


class EqFabric:

    def __init__(self, noAuth: bool = False):
        self.session = requests.Session()
        self.session.verify = False

        if not noAuth:
            self.authenticate()

    def authenticate(self, config=open("EqConfig.yml", 'r')):
        with config as stream:
            try:
                yamlDict = yaml.safe_load(stream)
                self.eiService = yamlDict['ei-service']
                userName = yamlDict["username"]
                passWord = yamlDict["password"]

                concat: str = userName + ":" + passWord
                bytes = concat.encode()
                payload = "Basic " + base64.b64encode(bytes).decode()

                res = self.session.post(
                    self.eiService + "api/login", headers={"Authorization": payload})

                self.session.headers["eiCookie"] = "eiToken=" + \
                    res.cookies.get("eiToken")

            except yaml.YAMLError as e:
                print(e)

    def getChartList(self, ids=True):
        try:
            res = self.session.get(self.eiService + "api/chart/list")
            data = res.json()["@graph"]
            if ids:
                idList = list(map(lambda chart: chart["@id"], data))
                return idList
            else:
                return data
        except RE.RequestException as e:
            print(e)

    def validate(self):
        try:
            res = self.session.post(self.eiService + "api/validate")
            print(res)
        except RE.RequestException as e:
            print(e)

    def deadDrops(self, df: pd.DataFrame, spaceInterval=0.05, timeInterval=5):
        try:
            dictList = df.to_dict('records')
            payload = json.dumps(dictList)
            params = {"spaceInterval": spaceInterval,
                      "timeInterval": timeInterval}
            res = self.session.post(
                self.eiService + "api/analytics/deadDrops", params=params, data=payload)
            odf = pd.DataFrame.from_dict(res.json())
            odf.rename(columns={"location-lat": "lat", "location-long": "long"}, inplace=True)
            return odf
        except RE.RequestException as e:
            print(e)

    def meetings(self, df: pd.DataFrame, spaceInterval=0.05, timeInterval=5):
        try:
            dictList = df.to_dict('records')
            payload = json.dumps(dictList)
            params = {"spaceInterval": spaceInterval,
                      "timeInterval": timeInterval}
            res = self.session.post(
                self.eiService + "api/analytics/meetings", params=params, data=payload)
            odf = pd.DataFrame.from_dict(res.json())
            odf.rename(columns={"location-lat": "lat", "location-long": "long"}, inplace=True)
            return odf
        except RE.RequestException as e:
            print(e)

    # types of inferrence numeric, temporal, geospatial, string

    def infer(
        self,
        longs: "list[float]",
        lats: "list[float]",
        times: "list[str]",
        ids: "list[str]",
        error: "list[float]",
        coincident: bool = True,
        spaceInterval: float = 1.0,
        timeInterval: float = 1.0,
    ):
        geobool = lats is not None and longs is not None and times is not None and ids is not None and error is not None

        if geobool:
            df = pd.DataFrame({
                "individual-local-identifier": ids,
                "location-long": longs,
                "location-lat": lats,
                "gps:dop": error,
                "timestamp": times
            })
            if coincident:
                return self.meetings(df, spaceInterval=spaceInterval, timeInterval=timeInterval)
            else:
                return self.deadDrops(df, spaceInterval=spaceInterval, timeInterval=timeInterval)

    @staticmethod
    def pretty_print_POST(req: requests.PreparedRequest):
        """
        At this point it is completely built and ready
        to be fired; it is "prepared".

        However pay attention at the formatting used in 
        this function because it is programmed to be pretty 
        printed and may differ from the actual request.
        """
        print('{}\n{}\r\n{}\r\n\r\n{}'.format(
            '-----------START-----------',
            req.method + ' ' + req.url,
            '\r\n'.join('{}: {}'.format(k, v) for k, v in req.headers.items()),
            req.body,
        ))
