using System;
using System.Text;
using System.IO;                        // for BinaryReader
using System.Data;
using WaterSimDCDC;                     // Model proper (constructor)
using System.Runtime.InteropServices;   // for DllImport
using System.Diagnostics;   // 
using System.Collections.Generic;

// 10.08.14

namespace WaterSim
{
    public class Model_5 : WaterSimU
    {
        /// <summary>  Tests model parameters, variables, inputs and outputs. </summary>
        /// <remarks>   David A Sampson 11/10/2015. </remarks>
        const string _APIVersion = "9.0";  // latest version of API
        string _ModelBuild = "";
        internal WaterSimManager_SIO ws;
        internal StreamWriter sw;
        DateTime now = DateTime.Now;

        bool invokeMyRun = true;
        ModelParameterClass DroughtScen;
        ModelParameterClass AgUrban;
        ModelParameterClass Pop;
        ModelParameterClass Personal;
        ModelParameterClass Env;
        ModelParameterClass Rec;
        //
        ProviderIntArray Out = new ProviderIntArray(0);

        //
        /// <summary>   Constructor. </summary>
        /// <remarks>   11/10/2015. </remarks>
        /// <param name="DataDirectoryName">    Pathname of the data directory. </param>
        /// <param name="TempDirectoryName">    Pathname of the temporary directory. </param>
        public Model_5(string DataDirectoryName, string TempDirectoryName)
        {
            // DataDirectoryName = @"C:\WaterSim\CSharp\API_10_0\API_Projects\WaterSimDCDC_WebService\WaterSimWbeService PHX Version 4\WaterSimWebServiceVersion3\App_Data\WaterSim_5_0\";
            //  DataDirectoryName = @"C:\WaterSim\CSharp\API_10_0\API_Projects\WaterSimDCDC_WebService\WaterSimWbeService PHX Version 4\WaterSimWebServiceVersion3\";

            WaterSimManager.CreateModelOutputFiles = true;
            //        
            ws = new WaterSimManager_SIO(DataDirectoryName, TempDirectoryName);

            if (invokeMyRun)
            {
                StreamW(TempDirectoryName);
                Run();
            }
        }
        #region myOutputs
        /// <summary>
        /// Run the model
        /// </summary>
        /// <param name="sw"></param>
        int[] GPCD = new int[33];
        //public void Run(StreamWriter sw)
        public void Run()
        {
            //ws.Simulation_Initialize();
            // ws.ParamManager.Model_Parameter("REGRECEFF").Value = 10;

            if (get_ValidModelRun == true)
            {
                string ver = ws.Model_Version;
                string buid = ws.ModelBuild;
                //for (int reg = 84; reg < 85; reg++)
                //{
                ws.Simulation_Initialize();
                Init();
                ws.Simulation_End_Year = 2061;

                //     setting
                 //ws.PCT_Wastewater_Reclaimed[API.ph] = 70;
                 //Rec = ws.ParamManager.Model_Parameter(eModelParam.epWebReclaimedWater_PCT);
                 //Rec.Value = 30;

                //ws.Colorado_Climate_Adjustment_Percent = 50;
                //ws.SaltVerde_Climate_Adjustment_Percent = 100;
                //ws.Colorado_User_Adjustment_Percent = 70;
                //ws.Colorado_User_Adjustment_StartYear = 2020;
                //ws.Colorado_User_Adjustment_Stop_Year = 2030;

                ws.ParamManager.Model_Parameter("REGRECEFF").Value = 100;
                 ws.ParamManager.Model_Parameter("WEBAGTR1").Value = 100;
                    // Ag efficiency
                ws.ParamManager.Model_Parameter("WEBAGEFF").Value = 90;
                //
               // ws.ParamManager.Model_Parameter("DROUSCEN").Value = 3;
                // ws.ParamManager.Model_Parameter("REGRECEFF").Value = reg;
                //   ws.ParamManager.Model_Parameter("WEBOUTDOOR").Value = 60;
                for (int i = 0; i < ProviderClass.NumberOfProviders; i++) { OneHundred[i] = 20; }
                Out.Values = OneHundred;
              

                //     getting
                //.RegionalValue(eProvider.Regional)
                // + ws.Regional_Groundwater_Balance
                ws.API_Cleared = true;
                for (int year = ws.Simulation_Start_Year; year < ws.Simulation_End_Year; ++year)
                {
                    ws.Simulation_NextYear();
                    //RunOneYear();
                    StartSimulation = false;
                    sw.WriteLine(year
                        + ","
                        + ws.Colorado_River_Flow
                        + ","
                        + ws.SaltTonto_River_Flow
                        + ","
                        + ws.Verde_River_Flow
                        + ","
                        + ws.GPCD_Used.RegionalValue(eProvider.Regional)
                        + ","
                        + ws.Total_Demand.RegionalValue(eProvider.Regional)
                        //+ ws.PCT_Wastewater_Reclaimed[API.sc]
                        + ","
                        + ws.PCT_Wastewater_Reclaimed[API.ph]
                        + ","
                        + ws.PCT_Wastewater_Reclaimed[API.pe]

                     );
                }
                //}
            }
            CloseFiles();
            sw.Flush();
            sw.Close();
        }
        #endregion
        public void run()
        {
            for (int year = ws.Simulation_Start_Year; year < ws.Simulation_End_Year; ++year)
            {
                ws.Simulation_NextYear();
                StartSimulation = false;
            }
            CloseFiles();
        }
        void Init()
        {
            // ========================================
            bool all = false;
            parmAPIcleared = true;
            //
            //
            //
            if (all)
            {
                Rec = ws.ParamManager.Model_Parameter(eModelParam.epWebReclaimedWater_PCT);
                Rec.Value = 30;

                DroughtScen = ws.ParamManager.Model_Parameter(eModelParam.epProvider_DroughtScenarios);
                DroughtScen.Value = 0;

                AgUrban = ws.ParamManager.Model_Parameter(eModelParam.epWebUIAgriculture);
                AgUrban.Value = 100;
                //
                Env = ws.ParamManager.Model_Parameter(eModelParam.epEnvironmentalFlow_PCT);
                Env.Value = 50;
                Personal = ws.ParamManager.Model_Parameter(eModelParam.epWebUIPersonal_PCT);
                Personal.Value = 100;
                Pop = ws.ParamManager.Model_Parameter(eModelParam.epWebPop_GrowthRateAdj_PCT);
                Pop.Value = 100;
                //
            }
 
            //
            ws.Colorado_Historical_Extraction_Start_Year = 1922;
            ws.SaltVerde_Historical_Extraction_Start_Year = 1946;
            //
            set_parmIncludeMeteorology = false;
            set_parmIncludeMeteorology = true;
        }
        ProviderIntArray One = new ProviderIntArray(0);
        internal int[] OneHundred = new int[ProviderClass.NumberOfProviders];
        internal int[] zero = new int[ProviderClass.NumberOfProviders];
     
        // ===========================================================================
        void runOnly(int st, int co)
        {

            for (int year = ws.Simulation_Start_Year; year < ws.Simulation_End_Year; ++year)
            {
                //ws.Simulation_NextYear();
                RunOneYear();
                int balance = 0;
                int used = 0;
                for (int p = 0; p < 33; ++p)
                {
                    balance += ws.Groundwater_Bank_Balance[p];
                    used += ws.Groundwater_Bank_Used[p];

                }

                sw.WriteLine(year
                 + ","
                 + st
                 + ","
                 + co
                 + ","
                 + balance
                 + ","
                 + used



                );

                StartSimulation = false;
            }
        }

        // ===========================================================================



        // ==============================================================================
        public void StreamW(string TempDirectoryName)
        {
            string filename = string.Concat(TempDirectoryName + "Output" + now.Month.ToString()
                + now.Day.ToString() + now.Minute.ToString() + now.Second.ToString()
                + "_" + ".csv");
            sw = File.AppendText(filename);
        }
        public string APiVersion { get { return _APIVersion; } }
        /// <summary>
        /// Verson of the Fortran Model
        /// </summary>
        public string ModelBuild { get { return _ModelBuild; } }

    }
}
