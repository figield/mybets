# -*- coding: utf-8 -*-

#!/usr/bin/python
import requests
import unittest

class TestbetAPI(unittest.TestCase):

    def setUp(self):
        self.base_url = "http://localhost:8080"
        self.bet_url = "http://localhost:8080/bet/"
        self.json_headers ={"Content-Type" : "application/json", "Accept" : "application/json"}
        self.new_bet = {"title": "ABC", "some": "attributes"}
        self.new_bet2 = {"title": "DEF"}

    def test_get_on_root_returns_html_hello_world(self):
        resp = requests.get(self.base_url)
        self.assertEqual(resp.content, "<html><body>Hello new world</body></html>")

    def test_get_on_nonexisting_bet_returns_404(self):
        resp = requests.get(self.bet_url + '1235')
        self.assertEqual(resp.status_code, 404)

    def test_get_on_bet_returns_id_in_html(self):
        for id in 1,2,3:
            resp = requests.get(self.bet_url + str(id))
            self.assertEqual(resp.status_code, 200)
            self.assertEqual(resp.content, "<html><body>" + str(id) + "</body></html>")

    def test_get_on_bet_returns_id_in_json(self):
        for id in 1,2,3:
            resp =requests.get(self.bet_url + str(id), \
                    headers=self.json_headers)
            self.assertEqual(resp.status_code, 200)
            self.assertEqual(resp.content, '{"id":' + '"' + str(id) + '",'\
                    '"title":'+ '"' + str(id) + '"}')

    def test_put_new_bet(self):
        url = self.bet_url + '0'
        # delete the first if present
        r = requests.delete(url)

        resp = requests.put(url, data=self.new_bet, headers=self.json_headers)
        self.assertEqual(resp.status_code, 201)
        self.assertEqual(resp.content, '{"id":"0","title":"ABC"}')

        # # Test the modification
        resp2 = requests.get(url)
        self.assertEqual(resp2.status_code, 200)
        self.assertEqual(resp2.content, "<html><body>0</body></html>")

    def test_put_updates_bet(self):
        url = self.bet_url + '0'
        resp = requests.get(url)
        # bet exists
        self.assertEqual(resp.status_code, 200)

        resp2 = requests.put(url, data=self.new_bet2, headers=self.json_headers)
        self.assertEqual(resp2.status_code, 200)
        self.assertEqual(resp2.content, '{"id":"0","title":"DEF"}')
        requests.delete(url)


    def test_delete_bet(self):
        # create it first
        url = self.bet_url + '0'

        resp = requests.put(url, data=self.new_bet, headers=self.json_headers)
        self.assertEqual(resp.status_code, 201)

        resp1 = requests.delete(url)
        self.assertEqual(resp1.status_code, 204)

        # test if durable
        resp2 = requests.get(url)
        self.assertEqual(resp2.status_code, 404)


    def test_post_new_bet(self):
        resp = requests.post(self.bet_url, data=self.new_bet,
                headers=self.json_headers)
        self.assertEqual(resp.status_code, 201)

        self.assertNotEqual(resp.content, '')

    def test_post_specific_bet_creates_another(self):
        url = self.bet_url + '1'

        resp = requests.post(url, data=self.new_bet, headers=self.json_headers)
        self.assertEqual(resp.status_code, 201)
        self.assertEqual(resp.content,  '{"id":"1","title":"ABC"}')
        # new one was created
        self.assertNotEqual(resp.headers["location"], None)
        # the new one is not the one which the POSt was apllied to
        self.assertNotEqual(resp.headers["location"], self.bet_url+'1')


    def test_put_without_data_is_malformed(self):
        url = self.bet_url + '1'
        resp = requests.put(url, data = {}, headers=self.json_headers)
        self.assertEqual(resp.status_code, 400)


if __name__ == "__main__":
    suite = unittest.TestLoader(verbosity=2).loadTestsFromTestCase(TestbetAPI)
    unittest.TextTestRunner.run(suite)
