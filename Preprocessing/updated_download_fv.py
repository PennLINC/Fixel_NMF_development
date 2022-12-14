from time import time, sleep
import multiprocessing
import flywheel
import datetime
import csv
from pathlib import Path
import pandas as pd

now = datetime.datetime.now().strftime("%Y-%m-%d_%H:%M")
fw = flywheel.Client()
qsiprep = fw.lookup('gears/qsiprep-fw-hpc')
proj = fw.projects.find_first("label=PNC_CS_810336")
analysis_label = 'qsiprep_0.3.16_0.8.0RC3'
WAIT_MINUTES = 20
ZIP_DIR = '/cbica/projects/pnc_fixel_cs/data/qsiprep_outputs/zips'

analysis_ids = []
sessions = proj.sessions()
with open('participants_lists/listwholesample.csv', 'r') as l:
    reader=csv.reader(l)
    ws=list(reader)
flat_list = [item for sublist in ws for item in sublist]
test_sessions = [ses for ses in sessions if ses.subject.label in flat_list]


def expected_zip_name(ses):
    zip_dir = Path(ZIP_DIR)
    full_ses = fw.get(ses.id)
    this_analysis = [ana for ana in full_ses.analyses if analysis_label in ana.label]
    this_analysis = sorted(this_analysis, key=lambda x: ["created"], reverse=True) #in my case, some subjects had several analyses, so this will choose the most recent one to download
    most_recent = this_analysis.pop()
    if not most_recent:
        print("No analysis found!: ", full_ses.label)
        return None
    print("Analysis found on ", str(most_recent['created']), most_recent['_id'])
    outputs = [f for f in most_recent.files if f.name.endswith('.zip') and not f.name.endswith('.html.zip')]
    output = outputs[0]
    dest = (zip_dir / output.name).resolve()
    return dest, output


def download_results(dest, fw_output):
    dest = str(dest)
    print("Downloading", dest)
    fw_output.download(dest)
    print("Done")
    return dest


# Check if the analysis has finished. If so, and it hasn't been downloaded, download it.
download_status = []
for ses in test_sessions:
    for trynum in range(10):
        try:
            dest, fw_dl_object = expected_zip_name(ses)
            if dest:
                break
        except Exception:
            if trynum == 9:
                print("Too many failures to download", ses.label)
                continue

    info = {'subject': ses.subject.label, 'session': ses.label}

    # If we can't find a unique analysis object
    if dest is None:
        print(fw_dl_object)
        info['status'] = fw_dl_object
        download_status.append(info)
        continue

    # If the zip already exists, don't re-download
    if dest.exists():
        print(str(dest), 'already Downloaded')
        info['status'] = str(dest)
        download_status.append(info)
        continue

    # Download in a separate process
    download_proc = multiprocessing.Process(target=download_results, args=(dest,fw_dl_object))
    download_proc.start()
    t0 = time()
    wait_seconds = WAIT_MINUTES * 60

    timeout = False
    while True:
        if time() - t0 > wait_seconds:
            print("Timeout")
            download_proc.terminate()
            timeout = True
            break
        if download_proc.is_alive():
            sleep(1)
        else:
            print('process exited normally')
            break
    if timeout:
        info['status'] = 'TimedOut'
        if dest.exists():
            print("Successful download despite timeout")
            info['status'] = str(dest)
    elif download_proc.exitcode > 0:
        info['status'] = 'FlywheelError'
    download_status.append(info)

status = pd.DataFrame(download_status)
status.to_csv(now+"_status.csv", index=False)
