import flywheel
import datetime
import csv
from pathlib import Path

now = datetime.datetime.now().strftime("%Y-%m-%d_%H:%M")
fw = flywheel.Client() #make sure your CLI and flywheel-sdk are the most recent version
qsiprep = fw.lookup('gears/qsiprep-fw-hpc/0.3.16_0.8.0RC3') #this was the chosen version of QSIPrep
proj = fw.projects.find_first("label=PNC_CS_810336")
analysis_label = 'qsiprep_{}_{}'.format(qsiprep.gear.version, now)

inputs = {
    "freesurfer_license": proj.files[5]
}

config = {
            'output_resolution': 1.25,
            'dwi_denoise_window': 5,
            'do_reconall': False,
            'shoreline_iters':0,
            'intramodal_template_iters':0,
            'b0_threshold':100,
            'hmc_model': 'eddy',
	    'save_outputs': True,
	    'unringing_method': 'mrdegibbs',
	    'output_space': 'T1w',
            'combine_all_dwis': True,
	    'denoise_before_combining': True,
	    'sloppy': False,
            'force_spatial_normalization': True 
}

analysis_ids = []
sessions = proj.sessions()
with open('participants_lists/listws_100.csv', 'r') as l: #instead of running the whole pnc sample (N=1100 with available data), run these sub-lists of 100 participants at a time so that they are not queued eternally on flywheel
    test_sessions = [line.strip() for line in l]

sessions_to_run = [ses for ses in sessions if ses.subject.label in test_sessions] #if you are using sessions IDs and not subjects IDs, instead of ses.subject.label use 'ses.label'
fails = []
for ses in sessions_to_run:
    try:
        _id = qsiprep.run(analysis_label=analysis_label,
                          config=config, inputs=inputs, destination=ses)
        analysis_ids.append(_id)
    except Exception as e:
        print(e)
        fails.append(ses)

with open(analysis_label+"_jobs.txt", "w") as jobsfile:
    jobsfile.write("\n".join(analysis_ids))
